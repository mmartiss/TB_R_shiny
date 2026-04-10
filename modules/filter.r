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
    
    fluidRow(
      # stulpeliu salinimas
      column(4,
             tags$label("Remove columns"),
             uiOutput(ns("col_remove_ui")),
             actionButton(ns("btn_remove"), "Remove selected", class = "btn-danger btn-sm")
      ),
      
      # rikiavimas
      column(4,
             tags$label("Sort by"),
             uiOutput(ns("col_sort_ui")),
             radioButtons(ns("sort_dir"), NULL,
                          choices  = c("Ascending" = "asc", "Descending" = "desc"),
                          inline   = TRUE
             ),
             actionButton(ns("btn_sort"), "Sort", class = "btn-primary btn-sm")
      ),
      
      # filtravimas
      column(4,
             tags$label("Filter rows"),
             uiOutput(ns("col_filter_ui")),
             uiOutput(ns("filter_value_ui")),
             actionButton(ns("btn_filter"), "Filter", class = "btn-primary btn-sm")
      ),
      
      #aktyvus filtrai
      column(12,
             tags$label("Active filters"),
             uiOutput(ns("active_filters_ui"))
      )
    ),
    
    br(),
    
    fluidRow(
      column(12,
             actionButton(ns("btn_reset"), "Reset all", class = "btn-warning btn-sm"),
             downloadButton(ns("dl_csv"), "Download CSV", class = "btn-sm"),
             downloadButton(ns("dl_tsv"), "Download TSV", class = "btn-sm"),
             actionButton(ns("btn_use"), "Use for analysis", class = "btn-success btn-sm")
      )
    ),
    
    br(),
    DTOutput(ns("filtered_tbl"))
  )
}

filterServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    filtered_data <- reactiveVal(NULL)
    
    #saugau filtrus
    active_filters <- reactiveVal(list())
    
    # kai ateina nauji duomenys
    observe({
      req(data())
      filtered_data(data())
    })
    
    # dinaminis stulpeliu sarasas - salinimui
    output$col_remove_ui <- renderUI({
      req(filtered_data())
      selectizeInput(session$ns("cols_remove"), 
                     label = NULL, 
                     choices = names(filtered_data()),
                     multiple = TRUE
      
      #checkboxGroupInput(session$ns("cols_remove"), NULL,
      #                   choices = names(filtered_data())
      )
    })
    
    # dinaminis stulpeliu sarasas - rikiavimui
    output$col_sort_ui <- renderUI({
      req(filtered_data())
      selectizeInput(session$ns("col_sort"), NULL,
                  choices = names(filtered_data())
      )
    })
    
    # dinaminis stulpeliu sarasas - filtravimui
    output$col_filter_ui <- renderUI({
      req(filtered_data())
      selectizeInput(session$ns("col_filter"), NULL,
                  choices = names(filtered_data()),
                  multiple = FALSE
      )
    })
    
    # filtro reiksme pagal pasirinkta stulpeli
    output$filter_value_ui <- renderUI({
      req(filtered_data(), input$col_filter)
      
      col <- filtered_data()[[input$col_filter]]
      
      if (is.numeric(col)) {
        sliderInput(session$ns("filter_val"), NULL,
                    min   = min(col, na.rm = TRUE),
                    max   = max(col, na.rm = TRUE),
                    value = c(min(col, na.rm = TRUE), max(col, na.rm = TRUE))
        )
      } else {
        unique_vals <- sort(unique(as.character(col)))
        selectizeInput(session$ns("filter_val"), NULL,
                    choices  = c("(all)", unique_vals),
                    selected = "(all)"
        )
      }
    })
    
    # salinimas
    observeEvent(input$btn_remove, {
      req(filtered_data(), input$cols_remove)
      df <- filtered_data()
      df <- df[, !names(df) %in% input$cols_remove, drop = FALSE]
      filtered_data(df)
    })
    
    # rikiavimas
    observeEvent(input$btn_sort, {
      req(filtered_data(), input$col_sort)
      df <- filtered_data()
      idx <- order(df[[input$col_sort]],
                   decreasing = input$sort_dir == "desc",
                   na.last    = TRUE)
      filtered_data(df[idx, , drop = FALSE])
    })
    
    # filtravimas
    observeEvent(input$btn_filter, {
      req(input$col_filter, input$filter_val)
      
      filters <- active_filters()
      
      filters[[length(filters) + 1]] <- list(
        col = input$col_filter,
        val = input$filter_val
      )
      
      active_filters(filters)
    })
    
    # reset
    observeEvent(input$btn_reset, {
      req(data())
      filtered_data(data())
      active_filters(list())
    })
    
    observe({
      filters <- active_filters()
      
      lapply(seq_along(filters), function(i) {
        
        observeEvent(input[[paste0("remove_filter_", i)]], {
          current <- active_filters()
          
          if (length(current) >= i) {
            current <- current[-i]
            active_filters(current)
          }
        }, ignoreInit = TRUE)
        
      })
    })
    
    
    output$filtered_tbl <- renderDT({
      req(filtered_result())
      datatable(
        filtered_result(),
        options  = list(scrollX = TRUE, pageLength = 15),
        rownames = FALSE,
        filter   = "top"
      )
    })
    
    output$dl_csv <- downloadHandler(
      filename = function() paste0("data_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(filtered_data(), file, row.names = FALSE)
    )
    
    output$dl_tsv <- downloadHandler(
      filename = function() paste0("data_", Sys.Date(), ".tsv"),
      content  = function(file) write.table(filtered_data(), file, sep = "\t", row.names = FALSE)
    )
    
    #aktyviu fitlru rodymas
    output$active_filters_ui <- renderUI({
      filters <- active_filters()
      ns <- session$ns
      
      if (length(filters) == 0) {
        return(tags$em("No filters applied"))
      }
      
      tagList(
        lapply(seq_along(filters), function(i) {
          f <- filters[[i]]
          val <- paste(f$val, collapse = " - ")
          
          tags$div(
            style = "margin-bottom:5px;",
            
            tags$span(paste0(f$col, ": ", val)),
            
            actionButton(
              ns(paste0("remove_filter_", i)),
              label = "✖",
              class = "btn btn-xs btn-danger",
              style = "margin-left:10px; padding:2px 6px;"
            )
          )
        })
      )
    })
    
    
    filtered_result <- reactive({
      req(filtered_data())
      
      df <- filtered_data()
      filters <- active_filters()
      
      for (f in filters) {
        col <- df[[f$col]]
        
        if (is.numeric(col)) {
          df <- df[
            !is.na(col) &
              col >= f$val[1] &
              col <= f$val[2],
          ]
        } else {
          if (!"(all)" %in% f$val) {
            df <- df[as.character(col) == f$val, ]
          }
        }
      }
      
      df
    })
    
    
    
    list(
      data        = filtered_result,
      btn_use     = reactive(input$btn_use)
    )
    
  })

}