uploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("File Upload"),
    fileInput(ns("file"), "Choose files", multiple = TRUE),
    
    tags$label("Analysis type"),
    selectInput(ns("analysis_type"), NULL,
                choices = c(
                  "16s"            = "16s",
                  "TB-Profiler"    = "tbprofiler",
                  "AMRFinder+"     = "amrfinder",
                  "MLST"           = "mlst",
                  "Kraken2"        = "kraken2",
                  "Custom / Other" = "custom"
                )),
    
    uiOutput(ns("delimiter_ui")),
    
    checkboxInput(ns("header"), "First row is header", value = TRUE),
    checkboxInput(ns("stringsAsFactors"), "Strings as factors", value = FALSE),
    
    actionButton(ns("btn_load"), "Load", class = "btn-primary"),
    
    DTOutput(ns("preview_tbl"))
  )
}

uploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$delimiter_ui <- renderUI({
      req(input$file)
      ext <- tools::file_ext(input$file$name[1])
      
      if (ext != "json") {
        tagList(
          tags$label("Delimiter"),
          selectInput(session$ns("delimeter"), NULL,
                      choices = c(
                        "Comma (CSV)"  = ",",
                        "Tab (TSV)"    = "\t",
                        "Semicolon"    = ";",
                        "Pipe"         = "|"
                      ))
        )
      }
    })
    
    loaded_data <- eventReactive(input$btn_load, {
      req(input$file)
      
      files <- input$file
      
      data_list <- lapply(seq_len(nrow(files)), function(i) {
        
        path <- files$datapath[i]
        ext  <- tools::file_ext(files$name[i])
        
        if (ext == "json") {
          parsed <- jsonlite::fromJSON(path, flatten = FALSE)
          
          if (is.data.frame(parsed)) {
            df <- parsed
          } else if (is.list(parsed)) {
            scalars <- Filter(function(x) length(x) == 1, parsed)
            df <- as.data.frame(scalars, stringsAsFactors = FALSE)
          } else {
            df <- data.frame()
          }
          
        } else {
          sep <- if (is.null(input$delimeter)) "," else input$delimeter
          
          df <- read.delim(
            path,
            sep              = sep,
            header           = input$header,
            stringsAsFactors = input$stringsAsFactors
          )
        }
        
        df$source_file <- files$name[i]
        df
      })
      
      dplyr::bind_rows(data_list)
    })

    loaded_meta <- eventReactive(input$btn_load, {
      req(input$file)
      list(
        filename     = input$file$name, # vektorius, nebe [1]
        analysisType = input$analysis_type,
        delimeter    = input$delimeter
      )
    })
    
    output$preview_tbl <- renderDT({
      req(loaded_data())
      datatable(
        loaded_data(),
        options  = list(scrollX = TRUE, pageLength = 15),
        rownames = FALSE,
        filter   = "top"
      )
    })
    
    list(
      data = loaded_data,
      meta = loaded_meta
    )
  })
}