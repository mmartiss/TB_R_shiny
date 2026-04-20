metadataFilterUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        .filter-badge {
          display: inline-block;
          background-color: #d9edf7;
          color: #31708f;
          padding: 5px 10px;
          margin: 2px;
          border-radius: 4px;
          font-size: 12px;
          max-width: 200px;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
          vertical-align: middle;
        }
        .filter-container { margin-bottom: 15px; display: block; clear: both; }
      "))
    ),
    h3("Metadata Explorer & Filter"),
    wellPanel(
      fluidRow(
        column(3,
               tags$b("Column Management"),
               uiOutput(ns("col_remove_ui")),
               actionButton(ns("btn_remove"), "Remove Selected", class="btn-danger btn-sm")
        ),
        column(3,
               tags$b("Rename Columns"),
               uiOutput(ns("col_rename_select_ui")),
               textInput(ns("new_col_name"), NULL, placeholder="New name"),
               actionButton(ns("btn_rename"), "Rename", class="btn-info btn-sm")
        ),
        column(3,
               tags$b("Sorting"),
               uiOutput(ns("col_sort_ui")),
               radioButtons(ns("sort_dir"), NULL, c("Asc"="asc","Desc"="desc"), inline=T),
               actionButton(ns("btn_sort"), "Sort", class="btn-primary btn-sm")
        ),
        column(3,
               tags$b("Filter Rows"),
               uiOutput(ns("col_filter_ui")),
               uiOutput(ns("filter_value_ui")),
               actionButton(ns("btn_filter"), "Add Filter", class="btn-primary btn-sm")
        )
      ),
      hr(),
      fluidRow(
        # --- Batch Replace ---
        column(4,
               tags$b("Batch Replace"),
               uiOutput(ns("replace_col_ui")),
               uiOutput(ns("replace_val_old_ui")),
               textInput(ns("replace_val_new"), NULL, placeholder="New value"),
               actionButton(ns("btn_replace"), "Replace All", class="btn-warning btn-sm")
        ),
        
        # --- Global Value Filter (numeric + text) ---
        column(4,
               tags$b("Global Value Filter (Across Columns)"),
               uiOutput(ns("global_ignore_ui")),
               radioButtons(ns("global_mode"), "Filter type:",
                            choices = c("Numeric" = "numeric", "Text" = "text"),
                            inline = TRUE),
               conditionalPanel(
                 condition = paste0("input['", ns("global_mode"), "'] == 'numeric'"),
                 fluidRow(
                   column(6, selectInput(ns("global_op"), "Operator",
                                         choices = c(">" = ">", "<" = "<", "==" = "==",
                                                     ">=" = ">=", "<=" = "<="))),
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
        
        # --- Prideti stulpeli ---
        column(4,
               tags$b("Add New Column"),
               textInput(ns("new_col_name_add"), "Column name", placeholder = "e.g. group"),
               selectInput(ns("new_col_type"), "Fill with",
                           choices = c("Fixed value"      = "fixed",
                                       "Empty (NA)"       = "na",
                                       "Row numbers"      = "rownum",
                                       "Copy from column" = "copy")),
               conditionalPanel(
                 condition = paste0("input['", ns("new_col_type"), "'] == 'fixed'"),
                 textInput(ns("new_col_fixed_val"), NULL, placeholder = "Value for all rows")
               ),
               conditionalPanel(
                 condition = paste0("input['", ns("new_col_type"), "'] == 'copy'"),
                 uiOutput(ns("copy_col_src_ui"))
               ),
               actionButton(ns("btn_add_col"), "Add Column", class="btn-success btn-sm")
        )
      )
    ),
    
    tags$b("Active Metadata Filters:"),
    tags$div(class="filter-container", uiOutput(ns("active_filters_ui"))),
    br(),
    
    fluidRow(
      column(12,
             actionButton(ns("btn_reset"), "Reset Metadata", class="btn-warning btn-sm"),
             downloadButton(ns("dl_csv"), "Download CSV", class="btn-sm"),
             downloadButton(ns("dl_xlsx"), "Download Excel", class="btn-sm"),
             actionButton(ns("btn_use"), "Confirm Metadata for Analysis",
                          class="btn-success btn-sm", style="font-weight:bold;")
      )
    ),
    hr(),
    DTOutput(ns("tbl"))
  )
}

metadataFilterServer <- function(id, original_metadata) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    current_meta   <- reactiveVal(NULL)
    active_filters <- reactiveVal(list())
    
    shorten <- function(x, n = 25) {
      ifelse(nchar(x) > n, paste0(substr(x, 1, n - 3), "..."), x)
    }
    
    get_uid <- function() paste0("f_", as.numeric(Sys.time()) * 1000, floor(runif(1, 1, 1000)))
    
    get_choices <- reactive({
      req(current_meta())
      nms <- names(current_meta())
      setNames(nms, shorten(nms))
    })
    
    observe({
      req(original_metadata())
      current_meta(original_metadata())
    })
    
    # ── Batch Replace ──────────────────────────────────────────────────────────
    output$replace_col_ui <- renderUI({
      selectInput(ns("replace_col"), NULL, choices = get_choices())
    })
    
    output$replace_val_old_ui <- renderUI({
      req(input$replace_col)
      choices <- sort(unique(as.character(current_meta()[[input$replace_col]])))
      selectInput(ns("replace_val_old"), NULL, choices = setNames(choices, shorten(choices, 20)))
    })
    
    observeEvent(input$btn_replace, {
      req(input$replace_col, input$replace_val_old)
      df  <- current_meta()
      col <- input$replace_col
      df[[col]][as.character(df[[col]]) == input$replace_val_old] <- input$replace_val_new
      current_meta(df)
      showNotification(paste0("Replaced '", input$replace_val_old, "' -> '", input$replace_val_new, "'"), type = "message")
    })
    
    # ── Column Management ──────────────────────────────────────────────────────
    output$col_remove_ui <- renderUI({
      req(current_meta())
      selectizeInput(ns("cols_remove"), NULL, choices = get_choices(), multiple = TRUE,
                     options = list(placeholder = "Select columns"))
    })
    
    output$col_sort_ui <- renderUI({
      req(current_meta())
      selectInput(ns("col_sort"), NULL, choices = get_choices())
    })
    
    output$col_rename_select_ui <- renderUI({
      req(current_meta())
      selectInput(ns("col_to_rename"), NULL, choices = get_choices())
    })
    
    observeEvent(input$btn_rename, {
      req(input$col_to_rename, input$new_col_name)
      df <- current_meta()
      names(df)[names(df) == input$col_to_rename] <- input$new_col_name
      current_meta(df)
      updateTextInput(session, "new_col_name", value = "")
    })
    
    observeEvent(input$btn_remove, {
      req(input$cols_remove)
      df <- current_meta()
      df <- df[, !names(df) %in% input$cols_remove, drop = FALSE]
      current_meta(df)
    })
    
    observeEvent(input$btn_sort, {
      req(input$col_sort)
      df  <- current_meta()
      idx <- order(df[[input$col_sort]], decreasing = (input$sort_dir == "desc"), na.last = TRUE)
      current_meta(df[idx, , drop = FALSE])
    })
    
    # ── Filter Rows (stulpelio filtras) ───────────────────────────────────────
    output$col_filter_ui <- renderUI({
      req(current_meta())
      selectInput(ns("col_filter"), NULL, choices = get_choices())
    })
    
    output$filter_value_ui <- renderUI({
      req(current_meta(), input$col_filter)
      col_data <- current_meta()[[input$col_filter]]
      
      if (is.numeric(col_data)) {
        sliderInput(ns("filter_val"), NULL,
                    min   = min(col_data, na.rm = TRUE),
                    max   = max(col_data, na.rm = TRUE),
                    value = c(min(col_data, na.rm = TRUE), max(col_data, na.rm = TRUE)))
      } else {
        choices <- sort(unique(as.character(col_data)))
        selectizeInput(ns("filter_val"), NULL,
                       choices  = setNames(choices, shorten(choices, 20)),
                       multiple = TRUE,
                       options  = list(placeholder = "Select categories"))
      }
    })
    
    observeEvent(input$btn_filter, {
      req(input$col_filter, input$filter_val)
      f_id    <- get_uid()
      filters <- active_filters()
      filters[[f_id]] <- list(
        type = "column",
        col  = input$col_filter,
        val  = input$filter_val
      )
      active_filters(filters)
    })
    
    # ── Global Value Filter ────────────────────────────────────────────────────
    output$global_ignore_ui <- renderUI({
      req(current_meta())
      selectizeInput(ns("global_ignore_cols"), "Ignore these columns:",
                     choices  = names(current_meta()),
                     multiple = TRUE)
    })
    
    observeEvent(input$btn_global_filter, {
      f_id    <- get_uid()
      filters <- active_filters()
      
      if (isTRUE(input$global_mode == "text")) {
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
    
    # ── Add New Column ─────────────────────────────────────────────────────────
    output$copy_col_src_ui <- renderUI({
      req(current_meta())
      selectInput(ns("copy_col_src"), NULL, choices = get_choices())
    })
    
    observeEvent(input$btn_add_col, {
      req(input$new_col_name_add)
      col_name <- trimws(input$new_col_name_add)
      if (col_name == "") {
        showNotification("Please enter a column name.", type = "warning"); return()
      }
      df <- current_meta()
      if (col_name %in% names(df)) {
        showNotification(paste0("Column '", col_name, "' already exists."), type = "warning"); return()
      }
      df[[col_name]] <- switch(input$new_col_type,
                               "fixed"  = input$new_col_fixed_val,
                               "na"     = NA,
                               "rownum" = seq_len(nrow(df)),
                               "copy"   = { req(input$copy_col_src); df[[input$copy_col_src]] }
      )
      current_meta(df)
      updateTextInput(session, "new_col_name_add", value = "")
      showNotification(paste0("Column '", col_name, "' added."), type = "message")
    })
    
    # ── Filtru salinimas ───────────────────────────────────────────────────────
    observe({
      filters <- active_filters()
      lapply(names(filters), function(f_id) {
        observeEvent(input[[paste0("remove_", f_id)]], {
          current          <- active_filters()
          current[[f_id]] <- NULL
          active_filters(current)
        }, ignoreInit = TRUE, once = TRUE)
      })
    })
    
    output$active_filters_ui <- renderUI({
      filters <- active_filters()
      if (length(filters) == 0) return(tags$em("None"))
      
      tagList(lapply(names(filters), function(f_id) {
        f <- filters[[f_id]]
        label_text <- if (f$type == "column") {
          paste0("[Col] ", f$col, ": ", paste(f$val, collapse = ", "))
        } else if (f$mode == "text") {
          paste0("[Global text] ", f$text_op, ": '", f$val, "' (", f$logic, ")")
        } else {
          paste0("[Global num] ", f$op, " ", f$val, " (", f$logic, ")")
        }
        tags$div(
          style = "display:inline-block; vertical-align: middle; margin: 2px;",
          tags$div(class = "filter-badge", title = label_text, shorten(label_text, 40)),
          actionLink(ns(paste0("remove_", f_id)),
                     icon("times-circle"),
                     style = "color:#d9534f; font-size:16px; margin-left:2px; vertical-align: middle;")
        )
      }))
    })
    
    # ── Pagalbine teksto palyginimo funkcija ───────────────────────────────────
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
    
    # ── Filtravimo logika ──────────────────────────────────────────────────────
    filtered_metadata <- reactive({
      df      <- current_meta()
      req(df)
      filters <- active_filters()
      
      for (f in filters) {
        if (f$type == "column") {
          if (!f$col %in% names(df)) next
          col_vals <- df[[f$col]]
          if (is.numeric(col_vals)) {
            df <- df[!is.na(col_vals) & col_vals >= f$val[1] & col_vals <= f$val[2], ]
          } else {
            df <- df[as.character(col_vals) %in% f$val, ]
          }
          
        } else if (f$type == "global") {
          target_cols <- setdiff(names(df), f$ignore)
          
          if (f$mode == "numeric") {
            numeric_cols <- target_cols[sapply(df[target_cols], is.numeric)]
            if (length(numeric_cols) > 0) {
              temp_mat        <- df[, numeric_cols, drop = FALSE]
              comparison_func <- match.fun(f$op)
              logic_mat       <- comparison_func(temp_mat, f$val)
              logic_mat[is.na(logic_mat)] <- FALSE
              row_keep <- if (f$logic == "any") rowSums(logic_mat) > 0 else rowSums(logic_mat) == ncol(logic_mat)
              df <- df[row_keep, ]
            }
            
          } else if (f$mode == "text") {
            if (length(target_cols) > 0) {
              logic_mat <- sapply(target_cols, function(col) {
                apply_text_match(df[[col]], f$text_op, f$val, f$case_sens)
              })
              if (is.vector(logic_mat)) logic_mat <- matrix(logic_mat, ncol = 1)
              logic_mat[is.na(logic_mat)] <- FALSE
              row_keep <- if (f$logic == "any") rowSums(logic_mat) > 0 else rowSums(logic_mat) == ncol(logic_mat)
              df <- df[row_keep, ]
            }
          }
        }
      }
      df
    })
    
    # ── Reset ──────────────────────────────────────────────────────────────────
    observeEvent(input$btn_reset, {
      req(original_metadata())
      current_meta(original_metadata())
      active_filters(list())
    })
    
    # ── Lentele ───────────────────────────────────────────────────────────────
    output$tbl <- renderDT({
      req(filtered_metadata())
      datatable(filtered_metadata(),
                editable = "cell",
                options  = list(scrollX = TRUE, pageLength = 10),
                rownames = FALSE,
                filter   = "top")
    })
    
    # ── Atsisiuntimas ─────────────────────────────────────────────────────────
    output$dl_csv <- downloadHandler(
      filename = function() paste0("metadata_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(filtered_metadata(), file, row.names = FALSE)
    )
    
    output$dl_xlsx <- downloadHandler(
      filename = function() paste0("metadata_", Sys.Date(), ".xlsx"),
      content  = function(file) writexl::write_xlsx(filtered_metadata(), file)
    )
    
    return(list(
      data    = filtered_metadata,
      btn_use = reactive(input$btn_use)
    ))
  })
}