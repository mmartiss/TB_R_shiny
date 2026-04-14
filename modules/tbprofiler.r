tbprofilerUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("TB-Profiler Analysis"),
    actionButton(ns("btn_analyze"), "Run TB Analysis", class = "btn-success", icon = icon("play")),
    hr(),
    tabsetPanel(
      tabPanel("Summary", DTOutput(ns("summary_tbl"))),
      tabPanel("Drug Resistance", DTOutput(ns("dr_tbl"))),
      tabPanel("Lineage Plot", plotOutput(ns("plot_lineage"))),
      tabPanel("Resistance Plot", plotOutput(ns("plot_dr")))
    )
  )
}

tbprofilerServer <- function(id, files_info, type) {
  moduleServer(id, function(input, output, session) {
    
    # Pagalbinė funkcija vieno TB-Profiler JSON failo išpakavimui
    parse_tb_json <- function(path, filename) {
      json_data <- jsonlite::fromJSON(path, simplifyVector = FALSE)
      
      # 1. Pagrindinė info
      lineage_list <- json_data$lineage
      lineage_str <- if(length(lineage_list) > 0) lineage_list[[1]]$lineage else "N/A"
      
      summary_df <- data.frame(
        Sample = filename,
        Lineage = lineage_str,
        QC_Pass = if(!is.null(json_data$qc$pass)) json_data$qc$pass else "N/A",
        stringsAsFactors = FALSE
      )
      
      # 2. Atsparumas vaistams
      dr_list <- json_data$drug_resistance
      dr_df <- data.frame()
      if (length(dr_list) > 0) {
        dr_df <- do.call(rbind, lapply(names(dr_list), function(drug) {
          data.frame(
            Sample = filename,
            Drug = drug,
            Status = dr_list[[drug]],
            stringsAsFactors = FALSE
          )
        }))
      }
      
      list(summary = summary_df, dr = dr_df)
    }
    
    # Analizės logika
    results <- eventReactive(input$btn_analyze, {
      req(type() == "tbprofiler")
      req(files_info())
      
      f_info <- files_info()
      
      all_summaries <- list()
      all_dr <- list()
      
      withProgress(message = 'Parsing TB-Profiler JSONs...', value = 0, {
        for (i in 1:nrow(f_info)) {
          parsed <- parse_tb_json(f_info$datapath[i], f_info$name[i])
          all_summaries[[i]] <- parsed$summary
          all_dr[[i]] <- parsed$dr
          incProgress(1/nrow(f_info))
        }
      })
      
      list(
        summary = dplyr::bind_rows(all_summaries),
        dr = dplyr::bind_rows(all_dr)
      )
    })
    
    # Lentelės
    output$summary_tbl <- renderDT({
      req(results())
      datatable(results()$summary, options = list(scrollX = TRUE))
    })
    
    output$dr_tbl <- renderDT({
      req(results())
      datatable(results()$dr, options = list(scrollX = TRUE))
    })
    
    # Grafikai
    output$plot_lineage <- renderPlot({
      req(results())
      ggplot(results()$summary, aes(x = Lineage, fill = Lineage)) +
        geom_bar() +
        theme_minimal() +
        labs(title = "Lineage Distribution")
    })
    
    output$plot_dr <- renderPlot({
      req(results())
      dr_data <- results()$dr %>% filter(Status == "resistant")
      if(nrow(dr_data) == 0) return(NULL)
      
      ggplot(dr_data, aes(x = Drug, fill = Drug)) +
        geom_bar() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Resistance by Drug (Count of Resistant Samples)")
    })
  })
}