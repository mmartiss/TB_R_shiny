# upload$data()        # original
# ↓
# filtered_data()      # remove/sort
# ↓
# filtered_result()    # + visi filtrai
# ↓
# [Use for analysis]
# ↓
# analysis_data()      # snapshot



filterUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Data Explorer"),
    uiOutput(ns("main_ui"))
  )
}

filterServer <- function(id, data, abundance = NULL, samples = NULL, analysis_type = NULL) {
  moduleServer(id, function(input, output, session) {
    
    is_amplicon <- reactive({
      req(analysis_type())
      analysis_type() %in% c("16s", "its")
    })
    
    # ── Paprastos analizės logika (nepakeista) ──────────────────────────────────
    
    filtered_data   <- reactiveVal(NULL)
    active_filters  <- reactiveVal(list())
    
    observe({
      req(!is_amplicon(), data())
      filtered_data(data())
    })
    
    observeEvent(input$btn_remove, {
      req(!is_amplicon(), filtered_data(), input$cols_remove)
      df <- filtered_data()
      df <- df[, !names(df) %in% input$cols_remove, drop = FALSE]
      filtered_data(df)
    })
    
    observeEvent(input$btn_sort, {
      req(!is_amplicon(), filtered_data(), input$col_sort)
      df  <- filtered_data()
      idx <- order(df[[input$col_sort]], decreasing = input$sort_dir == "desc", na.last = TRUE)
      filtered_data(df[idx, , drop = FALSE])
    })
    
    observeEvent(input$btn_filter, {
      req(!is_amplicon(), input$col_filter, input$filter_val)
      filters <- active_filters()
      filters[[length(filters) + 1]] <- list(col = input$col_filter, val = input$filter_val)
      active_filters(filters)
    })
    
    observeEvent(input$btn_reset, {
      req(!is_amplicon(), data())
      filtered_data(data())
      active_filters(list())
    })
    
    observe({
      req(!is_amplicon())
      filters <- active_filters()
      lapply(seq_along(filters), function(i) {
        observeEvent(input[[paste0("remove_filter_", i)]], {
          current <- active_filters()
          if (length(current) >= i) { current <- current[-i]; active_filters(current) }
        }, ignoreInit = TRUE)
      })
    })
    
    filtered_result <- reactive({
      req(!is_amplicon(), filtered_data())
      df      <- filtered_data()
      filters <- active_filters()
      for (f in filters) {
        col <- df[[f$col]]
        if (is.numeric(col)) {
          df <- df[!is.na(col) & col >= f$val[1] & col <= f$val[2], ]
        } else {
          if (!"(all)" %in% f$val) df <- df[as.character(col) == f$val, ]
        }
      }
      df
    })
    
    # ── Amplikon analizės logika (16s / ITS) ───────────────────────────────────
    
    make_amplicon_state <- function(suffix) {
      list(
        data    = reactiveVal(NULL),
        filters = reactiveVal(list())
      )
    }
    
    ab_state  <- make_amplicon_state("ab")
    smp_state <- make_amplicon_state("smp")
    
    observe({
      req(is_amplicon(), abundance())
      ab_state$data(abundance())
    })
    
    observe({
      req(is_amplicon(), samples())
      smp_state$data(samples())
    })
    
    make_amplicon_result <- function(state) {
      reactive({
        req(state$data())
        df      <- state$data()
        filters <- state$filters()
        for (f in filters) {
          col <- df[[f$col]]
          if (is.numeric(col)) {
            df <- df[!is.na(col) & col >= f$val[1] & col <= f$val[2], ]
          } else {
            if (!"(all)" %in% f$val) df <- df[as.character(col) == f$val, ]
          }
        }
        df
      })
    }
    
    ab_result  <- make_amplicon_result(ab_state)
    smp_result <- make_amplicon_result(smp_state)
    
    make_panel_server <- function(suffix, state, original_data) {
      
      btn_remove  <- paste0("btn_remove_",  suffix)
      btn_sort    <- paste0("btn_sort_",    suffix)
      btn_filter  <- paste0("btn_filter_",  suffix)
      btn_reset   <- paste0("btn_reset_",   suffix)
      cols_remove <- paste0("cols_remove_", suffix)
      col_sort    <- paste0("col_sort_",    suffix)
      sort_dir    <- paste0("sort_dir_",    suffix)
      col_filter  <- paste0("col_filter_",  suffix)
      filter_val  <- paste0("filter_val_",  suffix)
      
      output[[paste0("col_remove_ui_", suffix)]] <- renderUI({
        req(state$data())
        selectizeInput(session$ns(cols_remove), NULL, choices = names(state$data()), multiple = TRUE)
      })
      
      output[[paste0("col_sort_ui_", suffix)]] <- renderUI({
        req(state$data())
        selectizeInput(session$ns(col_sort), NULL, choices = names(state$data()))
      })
      
      output[[paste0("col_filter_ui_", suffix)]] <- renderUI({
        req(state$data())
        selectizeInput(session$ns(col_filter), NULL, choices = names(state$data()), multiple = FALSE)
      })
      
      output[[paste0("filter_value_ui_", suffix)]] <- renderUI({
        req(state$data(), input[[col_filter]])
        col <- state$data()[[input[[col_filter]]]]
        if (is.numeric(col)) {
          sliderInput(session$ns(filter_val), NULL,
                      min = min(col, na.rm = TRUE), max = max(col, na.rm = TRUE),
                      value = c(min(col, na.rm = TRUE), max(col, na.rm = TRUE)))
        } else {
          unique_vals <- sort(unique(as.character(col)))
          selectizeInput(session$ns(filter_val), NULL,
                         choices = c("(all)", unique_vals), selected = "(all)")
        }
      })
      
      output[[paste0("active_filters_ui_", suffix)]] <- renderUI({
        filters <- state$filters()
        ns      <- session$ns
        if (length(filters) == 0) return(tags$em("No filters applied"))
        tagList(lapply(seq_along(filters), function(i) {
          f   <- filters[[i]]
          val <- paste(f$val, collapse = " - ")
          tags$div(style = "margin-bottom:5px;",
                   tags$span(paste0(f$col, ": ", val)),
                   actionButton(ns(paste0("remove_filter_", suffix, "_", i)), "✖",
                                class = "btn btn-xs btn-danger",
                                style = "margin-left:10px; padding:2px 6px;"))
        }))
      })
      
      observeEvent(input[[btn_remove]], {
        req(state$data(), input[[cols_remove]])
        df <- state$data()
        df <- df[, !names(df) %in% input[[cols_remove]], drop = FALSE]
        state$data(df)
      })
      
      observeEvent(input[[btn_sort]], {
        req(state$data(), input[[col_sort]])
        df  <- state$data()
        idx <- order(df[[input[[col_sort]]]], decreasing = input[[sort_dir]] == "desc", na.last = TRUE)
        state$data(df[idx, , drop = FALSE])
      })
      
      observeEvent(input[[btn_filter]], {
        req(input[[col_filter]], input[[filter_val]])
        filters <- state$filters()
        filters[[length(filters) + 1]] <- list(col = input[[col_filter]], val = input[[filter_val]])
        state$filters(filters)
      })
      
      observeEvent(input[[btn_reset]], {
        req(original_data())
        state$data(original_data())
        state$filters(list())
      })
      
      observe({
        filters <- state$filters()
        lapply(seq_along(filters), function(i) {
          btn_id <- paste0("remove_filter_", suffix, "_", i)
          observeEvent(input[[btn_id]], {
            current <- state$filters()
            if (length(current) >= i) { current <- current[-i]; state$filters(current) }
          }, ignoreInit = TRUE)
        })
      })
    }
    
    make_panel_server("ab",  ab_state,  abundance)
    make_panel_server("smp", smp_state, samples)
    
    make_panel_ui <- function(suffix, title) {
      ns <- session$ns
      tagList(
        h4(title),
        fluidRow(
          column(4,
                 tags$label("Remove columns"),
                 uiOutput(ns(paste0("col_remove_ui_",  suffix))),
                 actionButton(ns(paste0("btn_remove_",  suffix)), "Remove selected", class = "btn-danger btn-sm")
          ),
          column(4,
                 tags$label("Sort by"),
                 uiOutput(ns(paste0("col_sort_ui_",    suffix))),
                 radioButtons(ns(paste0("sort_dir_",   suffix)), NULL,
                              choices = c("Ascending" = "asc", "Descending" = "desc"), inline = TRUE),
                 actionButton(ns(paste0("btn_sort_",   suffix)), "Sort",   class = "btn-primary btn-sm")
          ),
          column(4,
                 tags$label("Filter rows"),
                 uiOutput(ns(paste0("col_filter_ui_",  suffix))),
                 uiOutput(ns(paste0("filter_value_ui_", suffix))),
                 actionButton(ns(paste0("btn_filter_", suffix)), "Filter", class = "btn-primary btn-sm")
          ),
          column(12,
                 tags$label("Active filters"),
                 uiOutput(ns(paste0("active_filters_ui_", suffix)))
          )
        ),
        br(),
        fluidRow(
          column(12,
                 actionButton(ns(paste0("btn_reset_",  suffix)), "Reset all",       class = "btn-warning btn-sm"),
                 downloadButton(ns(paste0("dl_csv_",   suffix)), "Download CSV",    class = "btn-sm"),
                 downloadButton(ns(paste0("dl_tsv_",   suffix)), "Download TSV",    class = "btn-sm"),
                 actionButton(ns(paste0("btn_use_",    suffix)), "Use for analysis",class = "btn-success btn-sm")
          )
        ),
        br(),
        DTOutput(ns(paste0("tbl_", suffix)))
      )
    }
    
    make_panel_downloads <- function(suffix, result_fn) {
      output[[paste0("dl_csv_", suffix)]] <- downloadHandler(
        filename = function() paste0("data_", suffix, "_", Sys.Date(), ".csv"),
        content  = function(file) write.csv(result_fn(), file, row.names = FALSE)
      )
      output[[paste0("dl_tsv_", suffix)]] <- downloadHandler(
        filename = function() paste0("data_", suffix, "_", Sys.Date(), ".tsv"),
        content  = function(file) write.table(result_fn(), file, sep = "\t", row.names = FALSE)
      )
      output[[paste0("tbl_", suffix)]] <- renderDT({
        req(result_fn())
        datatable(result_fn(), options = list(scrollX = TRUE, pageLength = 15),
                  rownames = FALSE, filter = "top")
      })
    }
    
    make_panel_downloads("ab",  ab_result)
    make_panel_downloads("smp", smp_result)
    
    # ── Bendras UI ─────────────────────────────────────────────────────────────
    
    output$main_ui <- renderUI({
      if (is_amplicon()) {
        tagList(
          make_panel_ui("ab",  "Abundance"),
          tags$hr(),
          make_panel_ui("smp", "Samples")
        )
      } else {
        ns <- session$ns
        tagList(
          fluidRow(
            column(4,
                   tags$label("Remove columns"),
                   uiOutput(ns("col_remove_ui")),
                   actionButton(ns("btn_remove"), "Remove selected", class = "btn-danger btn-sm")
            ),
            column(4,
                   tags$label("Sort by"),
                   uiOutput(ns("col_sort_ui")),
                   radioButtons(ns("sort_dir"), NULL,
                                choices = c("Ascending" = "asc", "Descending" = "desc"), inline = TRUE),
                   actionButton(ns("btn_sort"), "Sort", class = "btn-primary btn-sm")
            ),
            column(4,
                   tags$label("Filter rows"),
                   uiOutput(ns("col_filter_ui")),
                   uiOutput(ns("filter_value_ui")),
                   actionButton(ns("btn_filter"), "Filter", class = "btn-primary btn-sm")
            ),
            column(12,
                   tags$label("Active filters"),
                   uiOutput(ns("active_filters_ui"))
            )
          ),
          br(),
          fluidRow(
            column(12,
                   actionButton(ns("btn_reset"), "Reset all",        class = "btn-warning btn-sm"),
                   downloadButton(ns("dl_csv"),  "Download CSV",     class = "btn-sm"),
                   downloadButton(ns("dl_tsv"),  "Download TSV",     class = "btn-sm"),
                   actionButton(ns("btn_use"),   "Use for analysis", class = "btn-success btn-sm")
            )
          ),
          br(),
          DTOutput(ns("filtered_tbl"))
        )
      }
    })
    
    output$col_remove_ui <- renderUI({
      req(!is_amplicon(), filtered_data())
      selectizeInput(session$ns("cols_remove"), NULL, choices = names(filtered_data()), multiple = TRUE)
    })
    output$col_sort_ui <- renderUI({
      req(!is_amplicon(), filtered_data())
      selectizeInput(session$ns("col_sort"), NULL, choices = names(filtered_data()))
    })
    output$col_filter_ui <- renderUI({
      req(!is_amplicon(), filtered_data())
      selectizeInput(session$ns("col_filter"), NULL, choices = names(filtered_data()), multiple = FALSE)
    })
    output$filter_value_ui <- renderUI({
      req(!is_amplicon(), filtered_data(), input$col_filter)
      col <- filtered_data()[[input$col_filter]]
      if (is.numeric(col)) {
        sliderInput(session$ns("filter_val"), NULL,
                    min = min(col, na.rm = TRUE), max = max(col, na.rm = TRUE),
                    value = c(min(col, na.rm = TRUE), max(col, na.rm = TRUE)))
      } else {
        unique_vals <- sort(unique(as.character(col)))
        selectizeInput(session$ns("filter_val"), NULL,
                       choices = c("(all)", unique_vals), selected = "(all)")
      }
    })
    output$active_filters_ui <- renderUI({
      req(!is_amplicon())
      filters <- active_filters()
      ns      <- session$ns
      if (length(filters) == 0) return(tags$em("No filters applied"))
      tagList(lapply(seq_along(filters), function(i) {
        f   <- filters[[i]]
        val <- paste(f$val, collapse = " - ")
        tags$div(style = "margin-bottom:5px;",
                 tags$span(paste0(f$col, ": ", val)),
                 actionButton(ns(paste0("remove_filter_", i)), "✖",
                              class = "btn btn-xs btn-danger",
                              style = "margin-left:10px; padding:2px 6px;"))
      }))
    })
    output$filtered_tbl <- renderDT({
      req(!is_amplicon(), filtered_result())
      datatable(filtered_result(), options = list(scrollX = TRUE, pageLength = 15),
                rownames = FALSE, filter = "top")
    })
    output$dl_csv <- downloadHandler(
      filename = function() paste0("data_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(filtered_result(), file, row.names = FALSE)
    )
    output$dl_tsv <- downloadHandler(
      filename = function() paste0("data_", Sys.Date(), ".tsv"),
      content  = function(file) write.table(filtered_result(), file, sep = "\t", row.names = FALSE)
    )
    
    # ── Grąžinami duomenys ─────────────────────────────────────────────────────
    
    list(
      data       = filtered_result,
      abundance  = ab_result,
      samples    = smp_result,
      btn_use    = reactive(input$btn_use),
      btn_use_ab = reactive(input$btn_use_ab),
      btn_use_smp= reactive(input$btn_use_smp)
    )
  })
}