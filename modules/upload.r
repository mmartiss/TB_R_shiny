uploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("File Upload"),
    
    tags$label("Analysis type"),
    selectInput(ns("analysis_type"), NULL,
                choices = c(
                  "16s"            = "16s",
                  "ITS"            = "its",
                  "Custom / Other" = "custom"
                )),
    
    uiOutput(ns("file_inputs_ui")),
    
    #optional metadat3a
    fileInput(ns("metadata_file"), "Upload metadata file (optional)", multiple = FALSE),
    
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
      file_uploaded <- if (needs_abundance()) input$abundance_file else input$file
      req(file_uploaded)
      
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
    })
    
    read_files <- function(files) {
      sep <- if (is.null(input$delimeter)) "," else input$delimeter
      
      data_list <- lapply(seq_len(nrow(files)), function(i) {
        read.delim(
          files$datapath[i],
          sep              = sep,
          header           = input$header,
          stringsAsFactors = input$stringsAsFactors
        )
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
      req(!needs_abundance(), input$file)
      read_files(input$file)
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
    
    loaded_metadata_df <- eventReactive(input$btn_load, {
      if (is.null(input$metadata_file)) return(NULL)
      read_files(input$metadata_file)
    })
    
    loaded_meta_info <- eventReactive(input$btn_load, {
      list(
        analysisType   = input$analysis_type,
        delimeter      = input$delimeter,
        filename       = if (needs_abundance()) c(input$abundance_file$name, input$sample_files$name) else input$file$name,
        metadata_file  = input$metadata_file$name
      )
    })
    
    output$tables_ui <- renderUI({
      tabs <- tagList()
      
      if (needs_abundance()) {
        req(loaded_abundance(), loaded_samples())
        tabs <- tagAppendChildren(tabs,
                                  h4("Abundance"), DTOutput(session$ns("abundance_tbl")),
                                  tags$br(),
                                  h4("Samples"), DTOutput(session$ns("samples_tbl"))
        )
      } else {
        req(loaded_data())
        tabs <- tagAppendChildren(tabs, 
                                  h4("Data Preview"), DTOutput(session$ns("preview_tbl"))
        )
      }
      
      # rodau tik jei ikelta ir buvo pridetas req (tai nepasileis su tusciu failu)
      if (!is.null(loaded_metadata_df())) {
        tabs <- tagAppendChildren(tabs,
                                  tags$hr(),
                                  h4("Metadata"), 
                                  DTOutput(session$ns("metadata_tbl"))
        )
      }
      
      tabs
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
    
    output$metadata_tbl <- renderDT({
      req(loaded_metadata_df())
      datatable(loaded_metadata_df(), options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE, filter = "top")
    })
    
    list(
      type      = reactive(input$analysis_type),
      abundance = loaded_abundance,
      samples   = loaded_samples,
      standard  = loaded_data,
      meta      = loaded_meta,
      metadata  = loaded_metadata_df,
      info      = loaded_meta_info,
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