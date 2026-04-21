filterUI <- function(id, title = "Data Table") {
  ns <- NS(id)
  tagList(
    # ==================================================
    # Filter module UI
    # ==================================================
    h4(title),
    fluidRow(
      # Row 1: column actions and row filters
      column(3, tags$label("Remove columns"), uiOutput(ns("col_remove_ui")), actionButton(ns("btn_remove"), "Remove", class="btn-danger btn-sm")),
      column(3, tags$label("Sort by"), uiOutput(ns("col_sort_ui")), radioButtons(ns("sort_dir"), NULL, c("Asc"="asc","Desc"="desc"), inline=T), actionButton(ns("btn_sort"), "Sort", class="btn-primary btn-sm")),
      column(3, tags$label("Rename Column"), uiOutput(ns("col_rename_select_ui")), textInput(ns("new_col_name"), NULL, placeholder="New name"), actionButton(ns("btn_rename"), "Rename", class="btn-info btn-sm")),
      column(3, tags$label("Batch Replace"), uiOutput(ns("replace_col_ui")), uiOutput(ns("replace_val_old_ui")), textInput(ns("replace_val_new"), NULL, placeholder="New value"), actionButton(ns("btn_replace"), "Replace All", class="btn-warning btn-sm")),
      column(3, tags$label("Filter by specific column"), uiOutput(ns("col_filter_ui")), uiOutput(ns("filter_value_ui")), actionButton(ns("btn_filter"), "Filter Column", class="btn-primary btn-sm")),
      
      # Row 2: global filter, read threshold, and taxonomy grouping
      column(4,
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
      ),
      column(3, 
             tags$label("Filter by Min Total Reads"),
             numericInput(ns("min_total_reads"), NULL, value = 10, min = 0),
             helpText("Removes rows where sum of all samples < threshold"),
             actionButton(ns("btn_read_filter"), "Filter Reads", class="btn-success btn-sm")
      ),
      column(3, 
             tags$label("Group by Taxonomy"),
             selectInput(ns("group_level"), NULL, 
                         choices = c("phylum", "class", "order", "family", "genus", "species")),
             actionButton(ns("btn_group_taxa"), "Group & Sum Counts", class="btn-info btn-sm"),
             helpText("This will sum all reads for each taxonomic group.")
      )
    ),
    br(),
    # Optional taxonomy join action
    uiOutput(ns("tax_join_ui")),
    br(),
    # Active filter summary
    tags$label("Active filters:"), uiOutput(ns("active_filters_ui")),
    br(),
    # Export and analysis handoff actions
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

    # ==================================================
    # Shared helpers
    # ==================================================
    
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

    # ==================================================
    # Filter registry and actions
    # ==================================================
    
    observeEvent(input$btn_group_taxa, {
      req(input$group_level)
      filters <- active_filters()

      new_filters <- list()
      for(id in names(filters)) {
        if (filters[[id]]$type != "group") new_filters[[id]] <- filters[[id]]
      }

      f_id <- get_uid()
      new_filters[[f_id]] <- list(
        type = "group",
        level = input$group_level
      )
      
      active_filters(new_filters)
    })
    
    observeEvent(input$btn_read_filter, {
      req(input$min_total_reads)
    # Add a minimum read-count filter
      f_id <- get_uid()
      filters <- active_filters()
      
      filters[[f_id]] <- list(
        type = "min_reads",
        threshold = input$min_total_reads
      )
      
      active_filters(filters)
    })
    
    # Show join control only when taxonomy data is available
    output$tax_join_ui <- renderUI({
      req(tax_data())
      actionButton(ns("btn_do_tax_join"), "Add Species Column", class = "btn-info btn-sm")
    })

    # Join taxonomy table and create a combined Species_Full label
    observeEvent(input$btn_do_tax_join, {
      df <- current_data()
      tax <- tax_data()
      req(df, tax)

      if (!"tax_id" %in% names(df)) {
        showNotification("The 'tax_id' column was not found in the table", type = "error")
        return()
      }

      df$tax_id <- as.character(df$tax_id)
      tax$tax_id <- as.character(tax$tax_id)

      res <- dplyr::left_join(df, tax, by = "tax_id")

      if ("genus" %in% names(res) && "species" %in% names(res)) {

        res$Species_Full <- paste(res$genus, res$species)
        res$Species_Full <- gsub("NA NA|NA|Unknown Unknown", "Unknown", res$Species_Full)

        tax_cols_to_remove <- setdiff(names(tax), "tax_id")

        res <- res[, !names(res) %in% tax_cols_to_remove]

        res <- res %>% dplyr::relocate(Species_Full, .after = tax_id)

        current_data(res)
        showNotification("The Species column has been successfully added next to tax_id", type = "message")

      } else {
        showNotification("The taxonomy file does not contain 'genus' or 'species' columns", type = "error")
      }
    })
    
    # Add a column-specific row filter
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
    
    # Add a global value filter across multiple columns
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
    
    # Remove active filter chips on demand
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
    
    # Human-readable active filter badges
    output$active_filters_ui <- renderUI({
      filters <- active_filters()
      if (length(filters) == 0) return(tags$em("None"))
      
      tagList(lapply(names(filters), function(f_id) {
        f <- filters[[f_id]]
        label <- "Filter"
        
        if (identical(f$type, "column")) {
          label <- paste0("[Col] ", f$col)
        } else if (identical(f$type, "min_reads")) {
          label <- paste0("[Min Total] >= ", f$threshold)
        } else if (identical(f$type, "group")) {
          label <- paste0("[Grouped by] ", f$level)
        } else if (identical(f$type, "global")) {
          label <- if(identical(f$mode, "text")) "[Global Text]" else "[Global Num]"
        }
        
        tags$div(
          style = "display:inline-block; margin:2px;",
          tags$span(class="filter-badge", shorten(label, 30)),
          actionLink(ns(paste0("remove_", f_id)),
                     icon("times-circle"),
                     style="color:#d9534f; margin-left:4px;")
        )
      }))
    })
    
    # ==================================================
    # Filter engine (applies all active rules)
    # ==================================================
    filtered_result <- reactive({
      df <- current_data()
      req(df)
      filters <- active_filters()

      for (f in filters) {
        if (is.null(f$type) || f$type == "group") next

        if (f$type == "column") {
          if (!f$col %in% names(df)) next
          vals <- df[[f$col]]
          if (is.numeric(vals)) {
            df <- df[!is.na(vals) & vals >= f$val[1] & vals <= f$val[2], ]
          } else {
            df <- df[as.character(vals) %in% f$val, ]
          }
        } 

        else if (f$type == "global") {
          target_cols <- setdiff(names(df), f$ignore)
          if (length(target_cols) == 0) next
          
          if (f$mode == "numeric") {
            num_cols <- target_cols[vapply(df[target_cols], is.numeric, logical(1))]
            if (length(num_cols) > 0) {
              mat <- as.matrix(df[, num_cols, drop = FALSE])
              logic_mat <- match.fun(f$op)(mat, f$val)
              logic_mat[is.na(logic_mat)] <- FALSE
              keep <- if (f$logic == "any") rowSums(logic_mat) > 0 else rowSums(logic_mat) == ncol(logic_mat)
              df <- df[keep, ]
            }
          } else {
            logic_list <- lapply(target_cols, function(c) apply_text_match(df[[c]], f$text_op, f$val, f$case_sens))
            logic_mat <- do.call(cbind, logic_list)
            logic_mat[is.na(logic_mat)] <- FALSE
            keep <- if (f$logic == "any") rowSums(logic_mat) > 0 else rowSums(logic_mat) == ncol(logic_mat)
            df <- df[keep, ]
          }
        }

        else if (f$type == "min_reads") {
          num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
          num_cols <- setdiff(num_cols, c("tax_id", "Species_Full"))
          if (length(num_cols) > 0) {
            df <- df[rowSums(df[, num_cols, drop = FALSE], na.rm = TRUE) >= f$threshold, ]
          }
        }
      }

      group_filter <- NULL
      for (f in filters) {
        if (identical(f$type, "group")) {
          group_filter <- f
          break
        }
      }
      
      if (!is.null(group_filter)) {
        req(tax_data())
        level <- group_filter$level
        tax <- tax_data()

        if ("tax_id" %in% names(df) && "tax_id" %in% names(tax)) {
          df$tax_id <- as.character(df$tax_id)
          tax$tax_id <- as.character(tax$tax_id)

          tax_sub <- tax[, c("tax_id", level), drop = FALSE]
          df <- dplyr::left_join(df, tax_sub, by = "tax_id")

          df[[level]][is.na(df[[level]]) | df[[level]] == ""] <- "Unknown"

          num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
          sample_cols <- setdiff(num_cols, "tax_id")

          df <- df %>%
            dplyr::group_by(!!dplyr::sym(level)) %>%
            dplyr::summarise(dplyr::across(dplyr::all_of(sample_cols), sum, na.rm = TRUE), .groups = "drop")

          if (length(sample_cols) > 0) {
            total_reads <- rowSums(df[, sample_cols, drop = FALSE])
            df <- df[order(total_reads, decreasing = TRUE), ]
          }
        }
      }
      
      return(df)
    })

    # ==================================================
    # Table controls and dynamic selectors
    # ==================================================
    output$col_remove_ui <- renderUI({
      req(current_data())
      selectInput(ns("cols_to_remove"), NULL, choices = names(current_data()), multiple = TRUE)
    })

    output$col_sort_ui <- renderUI({
      req(filtered_result())
      selectInput(ns("col_sort"), NULL, choices = names(filtered_result()))
    })

    output$col_rename_select_ui <- renderUI({
      req(current_data())
      selectInput(ns("col_to_rename"), NULL, choices = names(current_data()))
    })

    output$replace_col_ui <- renderUI({
      req(current_data())
      selectInput(ns("replace_col"), NULL, choices = names(current_data()))
    })
    
    output$replace_val_old_ui <- renderUI({
      req(input$replace_col, current_data())
      vals <- sort(unique(as.character(current_data()[[input$replace_col]])))
      selectInput(ns("replace_val_old"), NULL, choices = vals)
    })

    output$col_filter_ui <- renderUI({
      req(filtered_result())
      selectInput(ns("col_filter"), NULL, choices = names(filtered_result()))
    })

    output$filter_value_ui <- renderUI({
      req(input$col_filter, filtered_result())
      df <- filtered_result()
      col_data <- df[[input$col_filter]]
      
      if (is.numeric(col_data)) {
        rng <- range(col_data, na.rm = TRUE)
        sliderInput(ns("filter_val"), "Range:", min = rng[1], max = rng[2], value = rng)
      } else {
        vals <- sort(unique(as.character(col_data)))
        selectizeInput(ns("filter_val"), "Values:", choices = vals, multiple = TRUE,
                       options = list(plugins = list('remove_button')))
      }
    })

    output$global_ignore_ui <- renderUI({
      req(current_data())
      selectizeInput(ns("global_ignore_cols"), NULL, 
                     choices = names(current_data()), multiple = TRUE,
                     options = list(plugins = list('remove_button')))
    })
    
    # Column removal
    observeEvent(input$btn_remove, {
      req(input$cols_to_remove)
      df <- current_data()
      df <- df[, !(names(df) %in% input$cols_to_remove), drop = FALSE]
      current_data(df)
    })
    
    observeEvent(input$btn_rename, {
      req(input$col_to_rename, input$new_col_name)
      df <- current_data()
      names(df)[names(df) == input$col_to_rename] <- input$new_col_name
      current_data(df)
    })
    
    observeEvent(input$btn_replace, {
      req(input$replace_col, input$replace_val_old)
      df <- current_data()
      col <- input$replace_col
      df[[col]][df[[col]] == input$replace_val_old] <- input$replace_val_new
      current_data(df)
    })
    
    # Sort the current table
    observeEvent(input$btn_sort, {
      req(input$col_sort)
      df <- current_data()
      ord <- if(input$sort_dir == "asc") order(df[[input$col_sort]]) else order(df[[input$col_sort]], decreasing = TRUE)
      current_data(df[ord, , drop = FALSE])
    })

    # Reset back to the original input table
    observeEvent(input$btn_reset, {
      req(original_data())
      current_data(original_data())
      active_filters(list())
    })
    
    # ==================================================
    # Outputs: table and exports
    # ==================================================
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
    
    # Data exposed to the parent app
    return(list(
      data    = filtered_result,
      btn_use = reactive(input$btn_use)
    ))
  })
}