filterUI <- function(id, title = "Data Table") {
  ns <- NS(id)
  tagList(
    h4(title),
    fluidRow(
      column(3, tags$label("Remove columns"), uiOutput(ns("col_remove_ui")), actionButton(ns("btn_remove"), "Remove", class="btn-danger btn-sm")),
      column(3, tags$label("Sort by"), uiOutput(ns("col_sort_ui")), radioButtons(ns("sort_dir"), NULL, c("Asc"="asc","Desc"="desc"), inline=T), actionButton(ns("btn_sort"), "Sort", class="btn-primary btn-sm")),
      column(3, tags$label("Rename Column"), uiOutput(ns("col_rename_select_ui")), textInput(ns("new_col_name"), NULL, placeholder="New name"), actionButton(ns("btn_rename"), "Rename", class="btn-info btn-sm")),
      column(3, tags$label("Batch Replace"), uiOutput(ns("replace_col_ui")), uiOutput(ns("replace_val_old_ui")), textInput(ns("replace_val_new"), NULL, placeholder="New value"), actionButton(ns("btn_replace"), "Replace All", class="btn-warning btn-sm")),
      column(3, tags$label("Filter by specific column"), uiOutput(ns("col_filter_ui")), uiOutput(ns("filter_value_ui")), actionButton(ns("btn_filter"), "Filter Column", class="btn-primary btn-sm")),
      
      column(4, style = "background-color: #f9f9f9; border-radius: 5px; padding: 10px;",
             tags$label("Global Value Filter (Across Columns)"),
             uiOutput(ns("global_ignore_ui")),
             radioButtons(ns("global_mode"), "Filter type:",
                          choices = c("Numeric" = "numeric", "Text" = "text"),
                          inline = TRUE),
             conditionalPanel(
               condition = paste0("input['", ns("global_mode"), "'] == 'numeric'"),
               fluidRow(
                 column(6, selectInput(ns("global_op"), "Operator", choices = c(">" = ">", "<" = "<", "==" = "==", ">=" = ">=", "<=" = "<="))),
                 column(6, numericInput(ns("global_val_input"), "Value", value = 0))
               )
             ),
             conditionalPanel(
               condition = paste0("input['", ns("global_mode"), "'] == 'text'"),
               selectInput(ns("global_text_op"), "Match type",
                           choices = c("Contains"        = "contains",
                                       "Equals"           = "equals",
                                       "Starts with"      = "starts",
                                       "Ends with"        = "ends",
                                       "Does not contain" = "not_contains")),
               textInput(ns("global_text_val"), "Text", placeholder = "Search text..."),
               checkboxInput(ns("global_text_case"), "Case sensitive", value = FALSE)
             ),
             radioButtons(ns("global_logic"), "Keep row if:", 
                          choices = c("Any column matches" = "any", "All columns match" = "all"), 
                          inline = TRUE),
             actionButton(ns("btn_global_filter"), "Apply Global Filter", class="btn-success btn-sm")
      )
    ),
    br(),
    uiOutput(ns("tax_join_ui")),
    br(),
    tags$label("Active filters:"), uiOutput(ns("active_filters_ui")),
    br(),
    fluidRow(
      column(12,
             actionButton(ns("btn_reset"), "Reset all", class="btn-warning btn-sm"),
             downloadButton(ns("dl_csv"), "CSV", class="btn-sm"),
             downloadButton(ns("dl_tsv"), "TSV", class="btn-sm"),
             actionButton(ns("btn_use"), "Use for analysis", class="btn-success btn-sm")
      )
    ),
    br(),
    DTOutput(ns("tbl"))
  )
}

filterServer <- function(id, original_data, tax_data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    current_data   <- reactiveVal(NULL)
    active_filters <- reactiveVal(list())
    
    observeEvent(original_data(), {
      req(original_data())
      current_data(original_data())
    }, once = TRUE)
    
    shorten <- function(x, n = 25) {
      ifelse(nchar(x) > n, paste0(substr(x, 1, n - 3), "..."), x)
    }
    
    get_uid <- function() {
      paste0("f_", as.numeric(Sys.time()) * 1000, floor(runif(1, 1, 1000)))
    }
    
    apply_text_match <- function(x, op, val, case_sens) {
      s <- as.character(x)
      v <- val
      if (!case_sens) { s <- tolower(s); v <- tolower(v) }
      
      switch(op,
             "contains"     = grepl(v, s, fixed = TRUE),
             "equals"       = s == v,
             "starts"       = startsWith(s, v),
             "ends"         = endsWith(s, v),
             "not_contains" = !grepl(v, s, fixed = TRUE),
             rep(FALSE, length(s))
      )
    }
    
    output$tax_join_ui <- renderUI({
      req(tax_data())
      actionButton(ns("btn_do_tax_join"), "Add Species Column", class = "btn-info btn-sm")
    })
    
    # Sujungimo veiksmas
    observeEvent(input$btn_do_tax_join, {
      df <- current_data()
      tax <- tax_data()
      req(df, tax)
      
      # 1. Patikriname ar yra tax_id
      if (!"tax_id" %in% names(df)) {
        showNotification("The 'tax_id' column was not found in the table", type = "error")
        return()
      }
      
      # 2. Paruošiame IDs
      df$tax_id <- as.character(df$tax_id)
      tax$tax_id <- as.character(tax$tax_id)
      
      # 3. Sujungiam duomenis
      res <- dplyr::left_join(df, tax, by = "tax_id")
      
      # 4. Sukuriam Species stulpelį ir išvalom nereikalingus taksonomijos stulpelius
      # Tikriname, ar taksonomijos faile yra reikiami stulpeliai (mažosiomis raidėmis)
      if ("genus" %in% names(res) && "species" %in% names(res)) {
        
        # Sukuriam apjungtą pavadinimą
        res$Species_Full <- paste(res$genus, res$species)
        res$Species_Full <- gsub("NA NA|NA|Unknown Unknown", "Unknown", res$Species_Full)
        
        # Surandame visus stulpelius, kurie atkeliavo iš 'tax' failo (išskyrus tax_id ir mūsų naują Species_Full)
        tax_cols_to_remove <- setdiff(names(tax), "tax_id")
        
        # Pašaliname tuos papildomus stulpelius (Phylum, Order ir t.t.), kad liktų tik Species_Full
        res <- res[, !names(res) %in% tax_cols_to_remove]
        
        # 5. PERKELIAME stulpelį šalia tax_id
        # Naudojame relocate funkciją (iš dplyr)
        res <- res %>% dplyr::relocate(Species_Full, .after = tax_id)
        
        # Atnaujiname duomenis
        current_data(res)
        showNotification("The Species column has been successfully added next to tax_id", type = "message")
        
      } else {
        showNotification("The taxonomy file does not contain 'genus' or 'species' columns", type = "error")
      }
    })
    
    
    observeEvent(input$btn_filter, {
      req(input$col_filter, input$filter_val)
      f_id <- get_uid()
      filters <- active_filters()
      
      filters[[f_id]] <- list(
        type = "column",
        col  = input$col_filter,
        val  = input$filter_val
      )
      
      active_filters(filters)
    })
    
    observeEvent(input$btn_global_filter, {
      f_id <- get_uid()
      filters <- active_filters()
      
      if (input$global_mode == "text") {
        req(input$global_text_val)
        filters[[f_id]] <- list(
          type      = "global",
          mode      = "text",
          text_op   = input$global_text_op,
          val       = input$global_text_val,
          case_sens = isTRUE(input$global_text_case),
          ignore    = input$global_ignore_cols,
          logic     = input$global_logic
        )
      } else {
        req(input$global_val_input, input$global_op)
        filters[[f_id]] <- list(
          type   = "global",
          mode   = "numeric",
          op     = input$global_op,
          val    = input$global_val_input,
          ignore = input$global_ignore_cols,
          logic  = input$global_logic
        )
      }
      
      active_filters(filters)
    })
    
    observe({
      filters <- active_filters()
      lapply(names(filters), function(f_id) {
        observeEvent(input[[paste0("remove_", f_id)]], {
          tmp <- active_filters()
          tmp[[f_id]] <- NULL
          active_filters(tmp)
        }, ignoreInit = TRUE, once = TRUE)
      })
    })
    
    output$active_filters_ui <- renderUI({
      filters <- active_filters()
      if (length(filters) == 0) return(tags$em("None"))
      
      tagList(lapply(names(filters), function(f_id) {
        f <- filters[[f_id]]
        
        label <- if (f$type == "column") {
          paste0("[Col] ", f$col, ": ", paste(f$val, collapse = ", "))
        } else if (f$mode == "text") {
          paste0("[Global text] ", f$text_op, ": '", f$val, "' (", f$logic, ")")
        } else {
          paste0("[Global num] ", f$op, " ", f$val, " (", f$logic, ")")
        }
        
        tags$div(
          style = "display:inline-block; margin:2px;",
          tags$span(class="filter-badge", shorten(label, 40)),
          actionLink(ns(paste0("remove_", f_id)),
                     icon("times-circle"),
                     style="color:#d9534f; margin-left:4px;")
        )
      }))
    })
    
    filtered_result <- reactive({
      df <- current_data()
      req(df)
      filters <- active_filters()
      
      for (f in filters) {
        
        if (f$type == "column") {
          if (!f$col %in% names(df)) next
          
          col_vals <- df[[f$col]]
          
          if (is.numeric(col_vals)) {
            df <- df[!is.na(col_vals) & 
                       col_vals >= f$val[1] & 
                       col_vals <= f$val[2], ]
          } else {
            df <- df[as.character(col_vals) %in% f$val, ]
          }
          
        } else if (f$type == "global") {
          
          target_cols <- setdiff(names(df), f$ignore)
          
          if (f$mode == "numeric") {
            numeric_cols <- target_cols[sapply(df[target_cols], is.numeric)]
            
            if (length(numeric_cols) > 0) {
              temp <- df[, numeric_cols, drop = FALSE]
              fun  <- match.fun(f$op)
              
              logic_mat <- fun(temp, f$val)
              logic_mat[is.na(logic_mat)] <- FALSE
              
              keep <- if (f$logic == "any") {
                rowSums(logic_mat) > 0
              } else {
                rowSums(logic_mat) == ncol(logic_mat)
              }
              
              df <- df[keep, ]
            }
            
          } else {
            logic_mat <- sapply(target_cols, function(col) {
              apply_text_match(df[[col]], f$text_op, f$val, f$case_sens)
            })
            
            if (is.vector(logic_mat)) {
              logic_mat <- matrix(logic_mat, ncol = 1)
            }
            
            logic_mat[is.na(logic_mat)] <- FALSE
            
            keep <- if (f$logic == "any") {
              rowSums(logic_mat) > 0
            } else {
              rowSums(logic_mat) == ncol(logic_mat)
            }
            
            df <- df[keep, ]
          }
        }
      }
      
      df
    })
    
    observeEvent(input$btn_reset, {
      req(original_data())
      current_data(original_data())
      active_filters(list())
    })
    
    output$tbl <- renderDT({
      req(filtered_result())
      datatable(filtered_result(),
                editable = "cell",
                options  = list(scrollX = TRUE, pageLength = 10),
                rownames = FALSE,
                filter   = "top")
    })
    
    output$dl_csv <- downloadHandler(
      filename = function() paste0("data_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(filtered_result(), file, row.names = FALSE)
    )
    
    output$dl_tsv <- downloadHandler(
      filename = function() paste0("data_", Sys.Date(), ".tsv"),
      content  = function(file) write.table(filtered_result(), file, sep="\t", row.names=FALSE, quote=FALSE)
    )
    
    return(list(
      data    = filtered_result,
      btn_use = reactive(input$btn_use)
    ))
  })
}