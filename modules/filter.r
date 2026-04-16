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
      
      # Globalus filtras
      column(4, style = "background-color: #f9f9f9; border-radius: 5px; padding: 10px;",
             tags$label("Global Value Filter (Across Columns)"),
             uiOutput(ns("global_ignore_ui")),
             fluidRow(
               column(6, selectInput(ns("global_op"), "Operator", choices = c(">" = ">", "<" = "<", "==" = "==", ">=" = ">=", "<=" = "<="))),
               column(6, numericInput(ns("global_val_input"), "Value", value = 0))
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
    active_filters <- reactiveVal(list()) # Čia dabar saugosime pavadintą sąrašą (named list)
    
    has_tax_reactive <- !is.null(tax_data) && is.function(tax_data)
    
    observe({
      req(original_data())
      current_data(original_data())
    })
    
    # Pagalbinė funkcija unikaliems ID generuoti
    get_uid <- function() paste0("f_", as.numeric(Sys.time())*1000, floor(runif(1, 1, 1000)))
    
    # Stulpelio filtras
    observeEvent(input$btn_filter, {
      req(input$col_filter, input$filter_val)
      f_id <- get_uid()
      filters <- active_filters()
      filters[[f_id]] <- list(
        type = "column",
        col = input$col_filter, 
        val = input$filter_val
      )
      active_filters(filters)
    })
    
    # Globalus filtras
    observeEvent(input$btn_global_filter, {
      req(input$global_val_input, input$global_op)
      f_id <- get_uid()
      filters <- active_filters()
      filters[[f_id]] <- list(
        type = "global",
        op = input$global_op,
        val = input$global_val_input,
        ignore = input$global_ignore_cols,
        logic = input$global_logic
      )
      active_filters(filters)
    })
    
    # Filtrų šalinimo stebėjimas (patobulintas)
    observe({
      filters <- active_filters()
      # Einame per kiekvieną filtrą ir sukuriame observerį tik tam ID
      lapply(names(filters), function(f_id) {
        observeEvent(input[[paste0("remove_", f_id)]], {
          current <- active_filters()
          current[[f_id]] <- NULL # Pašaliname tik konkretų elementą pagal ID
          active_filters(current)
        }, ignoreInit = TRUE, once = TRUE)
      })
    })
    
    # Aktyvių filtrų rodymas
    output$active_filters_ui <- renderUI({
      filters <- active_filters()
      if (length(filters) == 0) return(tags$em("No filters applied"))
      
      tagList(lapply(names(filters), function(f_id) {
        f <- filters[[f_id]]
        label <- if(f$type == "column") {
          paste0("[Col] ", f$col, ": ", paste(f$val, collapse=" - "))
        } else {
          paste0("[Global] All numeric ", f$op, " ", f$val, " (", f$logic, ")")
        }
        
        tags$div(style = "display: inline-block; margin: 2px 5px; background: #eee; padding: 2px 8px; border-radius: 4px; border: 1px solid #ccc;",
                 tags$span(label),
                 actionLink(ns(paste0("remove_", f_id)), " ✖", style = "color:red; font-weight:bold; margin-left: 5px; text-decoration: none;")
        )
      }))
    })
    
    # Duomenų filtravimo procesas
    filtered_result <- reactive({
      df <- current_data()
      req(df)
      filters <- active_filters()
      
      # Einame per filtrus (nesvarbu, kokie jų ID)
      for (f in filters) {
        if (f$type == "column") {
          if (!f$col %in% names(df)) next
          col_data <- df[[f$col]]
          if (is.numeric(col_data)) {
            df <- df[!is.na(col_data) & col_data >= f$val[1] & col_data <= f$val[2], ]
          } else {
            if (!"(all)" %in% f$val) df <- df[as.character(col_data) %in% f$val, ]
          }
        } else if (f$type == "global") {
          target_cols <- setdiff(names(df), f$ignore)
          numeric_cols <- target_cols[sapply(df[target_cols], is.numeric)]
          if (length(numeric_cols) > 0) {
            temp_mat <- df[, numeric_cols, drop = FALSE]
            comparison_func <- match.fun(f$op)
            logic_mat <- comparison_func(temp_mat, f$val)
            logic_mat[is.na(logic_mat)] <- FALSE
            row_keep <- if (f$logic == "any") rowSums(logic_mat) > 0 else rowSums(logic_mat) == length(numeric_cols)
            df <- df[row_keep, ]
          }
        }
      }
      df
    })
    
    # --- Likusios funkcijos (be pakeitimų) ---
    
    output$global_ignore_ui <- renderUI({
      req(current_data())
      selectizeInput(ns("global_ignore_cols"), "Ignore these columns:", choices = names(current_data()), multiple = TRUE)
    })
    
    output$tax_join_ui <- renderUI({
      if (!has_tax_reactive) return(NULL)
      req(tax_data())
      actionButton(ns("btn_add_species"), "Add Species Column from Taxonomy", class="btn-info btn-sm", icon = icon("plus"))
    })
    
    output$replace_col_ui <- renderUI({ selectInput(ns("replace_col"), NULL, choices = names(current_data())) })
    output$replace_val_old_ui <- renderUI({
      req(input$replace_col)
      choices <- sort(unique(as.character(current_data()[[input$replace_col]])))
      selectInput(ns("replace_val_old"), NULL, choices = choices)
    })
    
    observeEvent(input$btn_replace, {
      req(input$replace_col, input$replace_val_old)
      df <- current_data(); col <- input$replace_col
      df[[col]][as.character(df[[col]]) == input$replace_val_old] <- input$replace_val_new
      current_data(df)
    })
    
    output$col_remove_ui <- renderUI({ req(current_data()); selectizeInput(ns("cols_remove"), NULL, choices = names(current_data()), multiple = TRUE) })
    output$col_sort_ui <- renderUI({ req(current_data()); selectizeInput(ns("col_sort"), NULL, choices = names(current_data())) })
    output$col_filter_ui <- renderUI({ req(current_data()); selectizeInput(ns("col_filter"), NULL, choices = names(current_data()), multiple = FALSE) })
    
    output$filter_value_ui <- renderUI({
      req(current_data(), input$col_filter)
      col <- current_data()[[input$col_filter]]
      if (is.numeric(col)) {
        sliderInput(ns("filter_val"), NULL, min = min(col, na.rm = TRUE), max = max(col, na.rm = TRUE), value = c(min(col, na.rm = TRUE), max(col, na.rm = TRUE)))
      } else {
        unique_vals <- sort(unique(as.character(col))); selectizeInput(ns("filter_val"), NULL, choices = c("(all)", unique_vals), selected = "(all)")
      }
    })
    
    output$col_rename_select_ui <- renderUI({ req(current_data()); selectInput(ns("col_to_rename"), NULL, choices = names(current_data())) })
    
    observeEvent(input$btn_rename, {
      req(input$col_to_rename, input$new_col_name); df <- current_data()
      names(df)[names(df) == input$col_to_rename] <- input$new_col_name
      current_data(df); updateTextInput(session, "new_col_name", value = "")
    })
    
    observeEvent(input$btn_remove, {
      req(current_data(), input$cols_remove); df <- current_data()
      df <- df[, !names(df) %in% input$cols_remove, drop = FALSE]
      current_data(df)
    })
    
    observeEvent(input$btn_sort, {
      req(current_data(), input$col_sort); df <- current_data()
      idx <- order(df[[input$col_sort]], decreasing = input$sort_dir == "desc", na.last = TRUE)
      current_data(df[idx, , drop = FALSE])
    })
    
    observeEvent(input$btn_reset, {
      req(original_data())
      current_data(original_data())
    })
    
    # observeEvent(input$btn_add_species, {
    #   req(current_data(), tax_data()); df <- current_data(); tax <- tax_data()
    #   if ("species" %in% names(df)) return(); df$tax_id <- as.character(df$tax_id); tax$tax_id <- as.character(tax$tax_id)
    #   df_new <- dplyr::left_join(df, tax[, c("tax_id", "species")], by = "tax_id")
    #   current_data(df_new)
    # })
    
    observeEvent(input$btn_add_species, {
      req(current_data(), tax_data())
      df <- current_data()
      tax <- tax_data()
      
      if ("species" %in% names(df)) {
        showNotification("Species column already exists", type = "warning")
        return()
      }
      
      df$tax_id <- as.character(df$tax_id)
      tax$tax_id <- as.character(tax$tax_id)
      tax_to_join <- tax[, c("tax_id", "species")]
      
      df_new <- dplyr::left_join(df, tax_to_join, by = "tax_id")
      
      # Perkeliame species į pradžią
      all_cols <- names(df_new)
      other_cols <- all_cols[!all_cols %in% c("tax_id", "species")]
      df_new <- df_new[, c("tax_id", "species", other_cols), drop = FALSE]
      
      current_data(df_new)
      showNotification("Species column added", type = "message")
    })
    
    output$dl_csv <- downloadHandler(filename = function() paste0("data_", id, "_", Sys.Date(), ".csv"), content = function(file) write.csv(filtered_result(), file, row.names = FALSE))
    output$dl_tsv <- downloadHandler(filename = function() paste0("data_", id, "_", Sys.Date(), ".tsv"), content = function(file) write.table(filtered_result(), file, sep = "\t", row.names = FALSE, quote = FALSE))
    
    output$tbl <- renderDT({
      req(filtered_result())
      datatable(filtered_result(), editable = "cell", options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE, filter = "top")
    })
    
    list(data = filtered_result, btn_use = reactive(input$btn_use))
  })
}