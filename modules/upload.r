uploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("File Upload"),
    
    tags$label("Analysis type"),
    selectInput(ns("analysis_type"), NULL,
                choices = c(
                  "16s"            = "16s",
                  "ITS"            = "its",
                  "TB-Profiler"    = "tbprofiler",
                  "Custom / Other" = "custom"
                )),
    
    uiOutput(ns("file_inputs_ui")),
    
    uiOutput(ns("delimiter_ui")),
    
    checkboxInput(ns("header"), "First row is header", value = TRUE),
    checkboxInput(ns("stringsAsFactors"), "Strings as factors", value = FALSE),
    
    actionButton(ns("btn_load"), "Load", class = "btn-primary"),
    
    uiOutput(ns("tables_ui"))
  )
}

uploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    needs_abundance <- reactive({
      input$analysis_type %in% c("16s", "its")
    })
    
    output$file_inputs_ui <- renderUI({
      if (needs_abundance()) {
        tagList(
          fileInput(session$ns("abundance_file"), "Upload abundance file", multiple = FALSE),
          fileInput(session$ns("sample_files"), "Upload sample file(s) (required)", multiple = TRUE)
        )
      } else {
        fileInput(session$ns("file"), "Choose files", multiple = TRUE)
      }
    })
    
    output$delimiter_ui <- renderUI({
      if (needs_abundance()) {
        first_name <- input$abundance_file$name[1]
      } else {
        first_name <- input$file$name[1]
      }
      req(first_name)
      ext <- tools::file_ext(first_name)
      
      if (ext != "json") {
        tagList(
          tags$label("Delimiter"),
          selectInput(session$ns("delimeter"), NULL,
                      choices = c(
                        "Tab (TSV)"    = "\t",
                        "Comma (CSV)"  = ",",
                        "Semicolon"    = ";",
                        "Pipe"         = "|"
                      ))
        )
      }
    })
    
    read_files <- function(files) {
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
        
        #df$source_file <- files$name[i]
        df
      })
      dplyr::bind_rows(data_list)
    }
    
    loaded_abundance <- eventReactive(input$btn_load, {
      req(needs_abundance(), input$abundance_file)
      read_files(input$abundance_file)
    })
    
    loaded_samples <- eventReactive(input$btn_load, {
      req(needs_abundance(), input$sample_files)
      read_files(input$sample_files)
    })
    
    loaded_data <- eventReactive(input$btn_load, {
      if (needs_abundance()) {
        if (is.null(input$abundance_file)) {
          showNotification("Upload abundance file", type = "error")
          req(input$abundance_file)
        }
        if (is.null(input$sample_files)) {
          showNotification("Upload sample file", type = "error")
          req(input$sample_files)
        }
        NULL
      } else {
        req(input$file)
        read_files(input$file)
      }
    })
    
    loaded_meta <- eventReactive(input$btn_load, {
      list(
        analysisType   = input$analysis_type,
        delimeter      = input$delimeter,
        filename       = if (needs_abundance()) c(input$abundance_file$name, input$sample_files$name) else input$file$name,
        abundance_file = if (needs_abundance()) input$abundance_file$name else NULL,
        sample_files   = if (needs_abundance()) input$sample_files$name else NULL
      )
    })
    
    output$tables_ui <- renderUI({
      if (needs_abundance()) {
        req(loaded_abundance(), loaded_samples())
        tagList(
          h4("Abundance"),
          DTOutput(session$ns("abundance_tbl")),
          tags$br(),
          h4("Samples"),
          DTOutput(session$ns("samples_tbl"))
        )
      } else {
        req(loaded_data())
        DTOutput(session$ns("preview_tbl"))
      }
    })
    
    output$abundance_tbl <- renderDT({
      req(loaded_abundance())
      datatable(loaded_abundance(), options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE, filter = "top")
    })
    
    output$samples_tbl <- renderDT({
      req(loaded_samples())
      datatable(loaded_samples(), options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE, filter = "top")
    })
    
    output$preview_tbl <- renderDT({
      req(loaded_data())
      datatable(loaded_data(), options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE, filter = "top")
    })
    
    list(
      type      = reactive(input$analysis_type),
      abundance = loaded_abundance,
      samples   = loaded_samples,
      standard  = loaded_data,
      meta      = loaded_meta,
      data      = loaded_data,
      files     = reactive({ 
        if (needs_abundance()) {
          input$sample_files
        } else {
          input$file 
        }
      })
    )
  })
}