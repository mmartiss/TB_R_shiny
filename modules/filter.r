filterUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Data Explorer"),
    uiOutput(ns("main_ui"))
  )
}

filterServer <- function(id, data, abundance = NULL, samples = NULL, analysis_type = NULL, metadata = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    is_amplicon <- reactive({
      req(analysis_type())
      analysis_type() %in% c("16s", "its")
    })
    
    # ── Paprastos analizės logika ──────────────────────────────────────────────
    
    filtered_data   <- reactiveVal(NULL)
    active_filters  <- reactiveVal(list())
    
    observe({
      req(!is_amplicon(), data())
      filtered_data(data())
    })
    
    observeEvent(input$filtered_tbl_cell_edit, {
      info <- input$filtered_tbl_cell_edit
      df <- filtered_data()
      df[info$row, info$col + 1] <- info$value
      filtered_data(df)
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
    
    # TSV/CSV paprastai analizei
    output$dl_csv <- downloadHandler(
      filename = function() paste0("data_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(filtered_result(), file, row.names = FALSE)
    )
    output$dl_tsv <- downloadHandler(
      filename = function() paste0("data_", Sys.Date(), ".tsv"),
      content  = function(file) write.table(filtered_result(), file, sep = "\t", row.names = FALSE, quote = FALSE)
    )
    
    # ── Amplikon / Metadata logika ─────────────────────────────────────────────
    
    make_amplicon_state <- function(suffix) {
      list(
        data    = reactiveVal(NULL),
        filters = reactiveVal(list())
      )
    }
    
    ab_state   <- make_amplicon_state("ab")
    smp_state  <- make_amplicon_state("smp")
    meta_state <- make_amplicon_state("meta")
    
    observe({ req(is_amplicon(), abundance()); ab_state$data(abundance()) })
    observe({ req(is_amplicon(), samples()); smp_state$data(samples()) })
    observe({ req(metadata()); meta_state$data(metadata()) })
    
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
    
    ab_result   <- make_amplicon_result(ab_state)
    smp_result  <- make_amplicon_result(smp_state)
    meta_result <- make_amplicon_result(meta_state)
    
    make_panel_server <- function(suffix, state, original_data) {
      ns_internal <- session$ns
      
      output[[paste0("col_remove_ui_", suffix)]] <- renderUI({
        req(state$data())
        selectizeInput(ns_internal(paste0("cols_remove_", suffix)), NULL, choices = names(state$data()), multiple = TRUE)
      })
      
      output[[paste0("col_sort_ui_", suffix)]] <- renderUI({
        req(state$data())
        selectizeInput(ns_internal(paste0("col_sort_", suffix)), NULL, choices = names(state$data()))
      })
      
      output[[paste0("col_filter_ui_", suffix)]] <- renderUI({
        req(state$data())
        selectizeInput(ns_internal(paste0("col_filter_", suffix)), NULL, choices = names(state$data()), multiple = FALSE)
      })
      
      output[[paste0("filter_value_ui_", suffix)]] <- renderUI({
        req(state$data(), input[[paste0("col_filter_", suffix)]])
        col <- state$data()[[input[[paste0("col_filter_", suffix)]]]]
        if (is.numeric(col)) {
          sliderInput(ns_internal(paste0("filter_val_", suffix)), NULL,
                      min = min(col, na.rm = TRUE), max = max(col, na.rm = TRUE),
                      value = c(min(col, na.rm = TRUE), max(col, na.rm = TRUE)))
        } else {
          unique_vals <- sort(unique(as.character(col)))
          selectizeInput(ns_internal(paste0("filter_val_", suffix)), NULL,
                         choices = c("(all)", unique_vals), selected = "(all)")
        }
      })
      
      output[[paste0("col_rename_select_ui_", suffix)]] <- renderUI({
        req(state$data())
        selectInput(ns_internal(paste0("col_to_rename_", suffix)), NULL, choices = names(state$data()))
      })
      
      observeEvent(input[[paste0("btn_rename_", suffix)]], {
        req(input[[paste0("col_to_rename_", suffix)]], input[[paste0("new_col_name_", suffix)]])
        df <- state$data()
        names(df)[names(df) == input[[paste0("col_to_rename_", suffix)]]] <- input[[paste0("new_col_name_", suffix)]]
        state$data(df)
        updateTextInput(session, paste0("new_col_name_", suffix), value = "")
      })
      
      observeEvent(input[[paste0("tbl_", suffix, "_cell_edit")]], {
        info <- input[[paste0("tbl_", suffix, "_cell_edit")]]
        df <- state$data()
        df[info$row, info$col + 1] <- info$value 
        state$data(df)
      })
      
      observeEvent(input[[paste0("btn_filter_", suffix)]], {
        req(input[[paste0("col_filter_", suffix)]], input[[paste0("filter_val_", suffix)]])
        filters <- state$filters()
        filters[[length(filters) + 1]] <- list(col = input[[paste0("col_filter_", suffix)]], val = input[[paste0("filter_val_", suffix)]])
        state$filters(filters)
      })
      
      observeEvent(input[[paste0("btn_remove_", suffix)]], {
        req(state$data(), input[[paste0("cols_remove_", suffix)]])
        df <- state$data()
        df <- df[, !names(df) %in% input[[paste0("cols_remove_", suffix)]], drop = FALSE]
        state$data(df)
      })
      
      observeEvent(input[[paste0("btn_sort_", suffix)]], {
        req(state$data(), input[[paste0("col_sort_", suffix)]])
        df <- state$data()
        idx <- order(df[[input[[paste0("col_sort_", suffix)]]]], decreasing = input[[paste0("sort_dir_", suffix)]] == "desc", na.last = TRUE)
        state$data(df[idx, , drop = FALSE])
      })
      
      observeEvent(input[[paste0("btn_reset_", suffix)]], {
        req(original_data())
        state$data(original_data())
        state$filters(list())
      })
      
      output[[paste0("active_filters_ui_", suffix)]] <- renderUI({
        filters <- state$filters()
        if (length(filters) == 0) return(tags$em("No filters applied"))
        tagList(lapply(seq_along(filters), function(i) {
          f <- filters[[i]]
          tags$div(style = "margin-bottom:5px;",
                   tags$span(paste0(f$col, ": ", paste(f$val, collapse=" - "))),
                   actionButton(ns_internal(paste0("remove_filter_", suffix, "_", i)), "✖",
                                class = "btn btn-xs btn-danger", style = "margin-left:10px;"))
        }))
      })
      
      observe({
        filters <- state$filters()
        lapply(seq_along(filters), function(i) {
          observeEvent(input[[paste0("remove_filter_", suffix, "_", i)]], {
            current <- state$filters()
            if (length(current) >= i) { current <- current[-i]; state$filters(current) }
          }, ignoreInit = TRUE)
        })
      })
    }
    
    make_panel_server("ab",   ab_state,   abundance)
    make_panel_server("smp",  smp_state,  samples)
    make_panel_server("meta", meta_state, metadata)
    
    make_panel_ui <- function(suffix, title) {
      tagList(
        h4(title),
        fluidRow(
          column(3, tags$label("Remove columns"), uiOutput(ns(paste0("col_remove_ui_", suffix))), actionButton(ns(paste0("btn_remove_", suffix)), "Remove", class="btn-danger btn-sm")),
          column(3, tags$label("Sort by"), uiOutput(ns(paste0("col_sort_ui_", suffix))), radioButtons(ns(paste0("sort_dir_", suffix)), NULL, c("Asc"="asc","Desc"="desc"), inline=T), actionButton(ns(paste0("btn_sort_", suffix)), "Sort", class="btn-primary btn-sm")),
          column(3, tags$label("Rename Column"), uiOutput(ns(paste0("col_rename_select_ui_", suffix))), textInput(ns(paste0("new_col_name_", suffix)), NULL, placeholder="New name"), actionButton(ns(paste0("btn_rename_", suffix)), "Rename", class="btn-info btn-sm")),
          column(3, tags$label("Filter rows"), uiOutput(ns(paste0("col_filter_ui_", suffix))), uiOutput(ns(paste0("filter_value_ui_", suffix))), actionButton(ns(paste0("btn_filter_", suffix)), "Filter", class="btn-primary btn-sm"))
        ),
        br(),
        tags$label("Active filters:"), uiOutput(ns(paste0("active_filters_ui_", suffix))),
        br(),
        fluidRow(
          column(12,
                 actionButton(ns(paste0("btn_reset_",  suffix)), "Reset all", class="btn-warning btn-sm"),
                 downloadButton(ns(paste0("dl_csv_", suffix)), "CSV", class="btn-sm"),
                 downloadButton(ns(paste0("dl_tsv_", suffix)), "TSV", class="btn-sm"), # Pridėtas TSV
                 actionButton(ns(paste0("btn_use_",    suffix)), "Use for analysis", class="btn-success btn-sm")
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
      # Pridėtas TSV handleris
      output[[paste0("dl_tsv_", suffix)]] <- downloadHandler(
        filename = function() paste0("data_", suffix, "_", Sys.Date(), ".tsv"),
        content  = function(file) write.table(result_fn(), file, sep = "\t", row.names = FALSE, quote = FALSE)
      )
      output[[paste0("tbl_", suffix)]] <- renderDT({
        req(result_fn())
        datatable(result_fn(), editable = "cell", options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE, filter = "top")
      })
    }
    
    make_panel_downloads("ab",   ab_result)
    make_panel_downloads("smp",  smp_result)
    make_panel_downloads("meta", meta_result)
    
    output$main_ui <- renderUI({
      ui_list <- tagList()
      if (is_amplicon()) {
        ui_list <- tagAppendChildren(ui_list, make_panel_ui("ab", "Abundance"), tags$hr(), make_panel_ui("smp", "Samples"))
      } else {
        ui_list <- tagAppendChildren(ui_list,
                                     fluidRow(
                                       column(4, uiOutput(ns("col_remove_ui")), actionButton(ns("btn_remove"), "Remove", class="btn-danger btn-sm")),
                                       column(4, uiOutput(ns("col_sort_ui")), radioButtons(ns("sort_dir"), NULL, c("Asc"="asc","Desc"="desc"), inline=T), actionButton(ns("btn_sort"), "Sort", class="btn-primary btn-sm")),
                                       column(4, uiOutput(ns("col_filter_ui")), uiOutput(ns("filter_value_ui")), actionButton(ns("btn_filter"), "Filter", class="btn-primary btn-sm"))
                                     ),
                                     br(), uiOutput(ns("active_filters_ui")), br(),
                                     actionButton(ns("btn_reset"), "Reset", class="btn-warning btn-sm"),
                                     downloadButton(ns("dl_csv"), "CSV", class="btn-sm"),
                                     downloadButton(ns("dl_tsv"), "TSV", class="btn-sm"), # Pridėtas TSV paprastai analizei
                                     actionButton(ns("btn_use"), "Use for analysis", class="btn-success btn-sm"),
                                     br(), br(),
                                     DTOutput(ns("filtered_tbl"))
        )
      }
      if (!is.null(metadata())) {
        ui_list <- tagAppendChildren(ui_list, tags$hr(), make_panel_ui("meta", "Metadata"))
      }
      ui_list
    })
    
    # Paprastos lentelės renderiai
    output$col_remove_ui <- renderUI({ req(!is_amplicon(), filtered_data()); selectizeInput(ns("cols_remove"), "Remove columns", choices = names(filtered_data()), multiple = T) })
    output$col_sort_ui <- renderUI({ req(!is_amplicon(), filtered_data()); selectizeInput(ns("col_sort"), "Sort by", choices = names(filtered_data())) })
    output$col_filter_ui <- renderUI({ req(!is_amplicon(), filtered_data()); selectizeInput(ns("col_filter"), "Filter by", choices = names(filtered_data())) })
    output$filter_value_ui <- renderUI({
      req(!is_amplicon(), filtered_data(), input$col_filter)
      col <- filtered_data()[[input$col_filter]]
      if(is.numeric(col)) sliderInput(ns("filter_val"), NULL, min(col, na.rm=T), max(col, na.rm=T), c(min(col, na.rm=T), max(col, na.rm=T)))
      else selectizeInput(ns("filter_val"), NULL, c("(all)", sort(unique(as.character(col)))), "(all)")
    })
    output$active_filters_ui <- renderUI({
      req(!is_amplicon()); filters <- active_filters()
      if(length(filters)==0) return("No filters applied")
      tagList(lapply(seq_along(filters), function(i) tags$div(paste0(filters[[i]]$col, ": ", paste(filters[[i]]$val, collapse=" ")), actionButton(ns(paste0("remove_filter_", i)), "✖", class="btn-xs btn-danger"))))
    })
    output$filtered_tbl <- renderDT({
      req(!is_amplicon(), filtered_result())
      datatable(filtered_result(), editable = "cell", options = list(scrollX = TRUE), rownames = FALSE, filter = "top")
    })
    
    list(
      data        = filtered_result,
      abundance   = ab_result,
      samples     = smp_result,
      metadata    = meta_result,
      btn_use     = reactive(input$btn_use),
      btn_use_ab  = reactive(input$btn_use_ab),
      btn_use_smp = reactive(input$btn_use_smp),
      btn_use_meta= reactive(input$btn_use_meta)
    )
  })
}