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
      json_data <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) return(NULL))
      if (is.null(json_data)) return(NULL)
      
      # 1. Pagrindinė info
      lineage_list <- json_data$lineage
      lineage_str <- if(length(lineage_list) > 0) lineage_list[[1]]$lineage else "N/A"
      lineage_family <- if(length(lineage_list) > 0) lineage_list[[1]]$family else "N/A"
      
      # Svarbu: naudojame drtype (be apatinio brūkšnio)
      dr_type_val <- if(!is.null(json_data$drtype)) json_data$drtype else "Unknown"
      
      summary_df <- data.frame(
        Sample = filename,
        DR_Type = dr_type_val,
        Lineage = lineage_str,
        Family = lineage_family,
        Coverage = if(!is.null(json_data$qc$median_coverage)) json_data$qc$median_coverage else NA,
        QC_Pass = if(!is.null(json_data$qc$pass)) as.character(json_data$qc$pass) else "N/A",
        stringsAsFactors = FALSE
      )
      
      # 2. Atsparumas vaistams (mutacijos)
      dr_variants <- json_data$dr_variants
      dr_df <- data.frame()
      if (length(dr_variants) > 0) {
        dr_list <- lapply(dr_variants, function(v) {
          data.frame(
            Sample = filename,
            # Išsaugome vaistus kaip sąrašą/vektorių vėlesniam apdorojimui
            Drug = if(!is.null(v$drugs)) paste(unlist(v$drugs), collapse = ", ") else "N/A",
            Gene = if(!is.null(v$gene)) v$gene else "N/A",
            Mutation = if(!is.null(v$mutation)) v$mutation else "N/A",
            Type = if(!is.null(v$type)) v$type else "N/A",
            stringsAsFactors = FALSE
          )
        })
        dr_df <- do.call(rbind, dr_list)
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
          if(!is.null(parsed)) {
            all_summaries[[i]] <- parsed$summary
            all_dr[[i]] <- parsed$dr
          }
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
    
    # Grafikas: Lineage
    output$plot_lineage <- renderPlot({
      req(results())
      ggplot(results()$summary, aes(x = Lineage, fill = DR_Type)) +
        geom_bar() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Lineage Distribution")
    })
    
    # Grafikas: Resistance (Pataisytas)
    output$plot_dr <- renderPlot({
      req(results())
      df_dr <- results()$dr
      
      # Patikriname, ar turime duomenų apie mutacijas
      if(nrow(df_dr) == 0) {
        return(ggplot() + annotate("text", x=0.5, y=0.5, label="No resistance mutations detected") + theme_void())
      }
      
      # Kadangi stulpelyje 'Drug' gali būti keli vaistai (pvz. "rifampicin, isoniazid"),
      # mums reikia juos išskirti į atskiras eilutes grafikui
      plot_df <- df_dr %>%
        tidyr::separate_rows(Drug, sep = ",\\s*") %>% # Išskaido "a, b" į dvi eilutes
        dplyr::filter(!is.na(Drug) & Drug != "N/A" & Drug != "")
      
      if(nrow(plot_df) == 0) {
        return(ggplot() + annotate("text", x=0.5, y=0.5, label="No drug-specific mutations found") + theme_void())
      }
      
      ggplot(plot_df, aes(x = Drug, fill = Drug)) +
        geom_bar() +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        ) +
        labs(
          title = "Prevalence of Resistance Mutations",
          x = "Drug",
          y = "Count of Mutations Found"
        )
    })
  })
}