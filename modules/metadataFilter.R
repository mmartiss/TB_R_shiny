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
               tags$label("Batch Replace"), 
               uiOutput(ns("replace_col_ui")), 
               uiOutput(ns("replace_val_old_ui")), 
               textInput(ns("replace_val_new"), NULL, placeholder="New value"), 
               actionButton(ns("btn_replace"), "Replace All", class="btn-warning btn-sm")
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
             actionButton(ns("btn_use"), "Confirm Metadata for Analysis", class="btn-success btn-sm", style="font-weight:bold;")
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
      ifelse(nchar(x) > n, paste0(substr(x, 1, n-3), "..."), x)
    }
    
    get_choices <- reactive({
      req(current_meta())
      nms <- names(current_meta())
      setNames(nms, shorten(nms))
    })
    
    observe({
      req(original_metadata())
      current_meta(original_metadata())
    })
    
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
      df <- current_meta()
      col <- input$replace_col
      df[[col]][as.character(df[[col]]) == input$replace_val_old] <- input$replace_val_new
      current_meta(df)
    })
    
    output$col_remove_ui <- renderUI({
      req(current_meta())
      selectizeInput(ns("cols_remove"), NULL, choices = get_choices(), multiple = TRUE, options = list(placeholder = 'Select columns'))
    })
    
    output$col_sort_ui <- renderUI({
      req(current_meta())
      selectInput(ns("col_sort"), NULL, choices = get_choices())
    })
    
    output$col_filter_ui <- renderUI({
      req(current_meta())
      selectInput(ns("col_filter"), NULL, choices = get_choices())
    })
    
    output$filter_value_ui <- renderUI({
      req(current_meta(), input$col_filter)
      col_data <- current_meta()[[input$col_filter]]
      
      if (is.numeric(col_data)) {
        sliderInput(ns("filter_val"), NULL,
                    min = min(col_data, na.rm = TRUE), max = max(col_data, na.rm = TRUE),
                    value = c(min(col_data, na.rm = TRUE), max(col_data, na.rm = TRUE)))
      } else {
        choices <- sort(unique(as.character(col_data)))
        selectizeInput(ns("filter_val"), NULL, choices = setNames(choices, shorten(choices, 20)), multiple = TRUE, options = list(placeholder = 'Select categories'))
      }
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
      df <- current_meta()
      idx <- order(df[[input$col_sort]], decreasing = (input$sort_dir == "desc"), na.last = TRUE)
      current_meta(df[idx, , drop = FALSE])
    })
    
    observeEvent(input$btn_filter, {
      req(input$col_filter, input$filter_val)
      filters <- active_filters()
      filters[[length(filters) + 1]] <- list(col = input$col_filter, val = input$filter_val)
      active_filters(filters)
    })
    
    observeEvent(input$btn_reset, {
      req(original_metadata())
      current_meta(original_metadata())
      active_filters(list())
    })
    
    observe({
      filters <- active_filters()
      lapply(seq_along(filters), function(i) {
        observeEvent(input[[paste0("remove_f_", i)]], {
          current <- active_filters()
          if (length(current) >= i) { 
            current <- current[-i]
            active_filters(current) 
          }
        }, ignoreInit = TRUE)
      })
    })
    
    output$active_filters_ui <- renderUI({
      filters <- active_filters()
      if (length(filters) == 0) return(tags$em("None"))
      tagList(lapply(seq_along(filters), function(i) {
        f <- filters[[i]]
        label_text <- paste0(f$col, ": ", paste(f$val, collapse=", "))
        tags$div(style = "display:inline-block; vertical-align: middle; margin: 2px;",
                 tags$div(class="filter-badge", title=label_text, shorten(label_text, 35)),
                 actionLink(ns(paste0("remove_f_", i)), icon("times-circle"), style="color:#d9534f; font-size:16px; margin-left:2px; vertical-align: middle;")
        )
      }))
    })
    
    filtered_metadata <- reactive({
      df <- current_meta()
      req(df)
      filters <- active_filters()
      for (f in filters) {
        if (!f$col %in% names(df)) next
        col_vals <- df[[f$col]]
        if (is.numeric(col_vals)) {
          df <- df[!is.na(col_vals) & col_vals >= f$val[1] & col_vals <= f$val[2], ]
        } else {
          df <- df[as.character(col_vals) %in% f$val, ]
        }
      }
      df
    })
    
    output$tbl <- renderDT({
      req(filtered_metadata())
      datatable(filtered_metadata(), 
                editable = "cell", 
                options = list(scrollX = TRUE, pageLength = 10), 
                rownames = FALSE, 
                filter = "top")
    })
    
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