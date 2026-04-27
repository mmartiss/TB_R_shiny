options(shiny.maxRequestSize = 100 * 1024^2) # for EMU tree file upload

uploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    # ==================================================
    # Upload UI
    # ==================================================
    h2("Emu Data Upload (16S/ITS)"),
    
    # Analysis mode selector
    tags$label("Analysis type"),
    selectInput(ns("analysis_type"), NULL,
                choices = c(
                  "16S"            = "16s",
                  "ITS"            = "its",
                  "Custom / Other" = "custom"
                )),
    
    # Standard Emu workflow: abundance, counts, taxonomy, metadata, tree
    conditionalPanel(
      condition = sprintf("input['%s'] == '16s' || input['%s'] == 'its'", ns("analysis_type"), ns("analysis_type")),
      wellPanel(
        h4("Required Emu Files"),
        fileInput(ns("abundance_file"), "1. Upload emu-combined-abundance", accept = ".tsv"),
        fileInput(ns("counts_file"), "2. Upload emu-combined-counts", accept = ".tsv"),
        fileInput(ns("taxonomy_file"), "3. Upload emu-combined-taxonomy", accept = ".tsv"),
        hr(),
        h4("Optional Files"),
        fileInput(ns("tree_file"), "4. Upload phylogenetic tree (.nwk)", accept = ".nwk"),
        fileInput(ns("metadata_file"), "5. Upload metadata file", 
                  accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls"))
      )
    ),
    
    # Custom workflow: multiple free-form files
    conditionalPanel(
      condition = sprintf("input['%s'] == 'custom'", ns("analysis_type")),
      fileInput(ns("custom_file"), "Choose files", multiple = TRUE)
    ),
    
    # Reader configuration and load action
    uiOutput(ns("delimeter_ui")),
    checkboxInput(ns("header"), "First row is header", value = TRUE),
    
    actionButton(ns("btn_load"), "Load Data", class = "btn-primary", icon = icon("upload")),
    
    # Preview tables rendered after loading
    uiOutput(ns("tables_ui"))
  )
}

uploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ==================================================
    # Shared helpers
    # ==================================================

    # Generic TSV/CSV reader used for Emu and metadata files
    read_emu_file <- function(file_info) {
      req(file_info)
      sep <- if (is.null(input$delimeter)) "\t" else input$delimeter
      read.delim(
        file_info$datapath,
        sep = sep,
        header = input$header,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
    
    # Delimiter selector shown in the UI
    output$delimeter_ui <- renderUI({
      tagList(
        selectInput(session$ns("delimeter"), "Delimiter",
                    choices = c("Tab (TSV)" = "\t", "Comma (CSV)" = ",", "Semicolon" = ";"))
      )
    })
    
    # ==================================================
    # Loaders
    # ==================================================

    # Required Emu inputs
    loaded_abundance <- eventReactive(input$btn_load, { read_emu_file(input$abundance_file) })
    loaded_counts    <- eventReactive(input$btn_load, { read_emu_file(input$counts_file) })
    loaded_taxonomy  <- eventReactive(input$btn_load, { read_emu_file(input$taxonomy_file) })

    # Optional metadata input; supports Excel and delimited text
    loaded_metadata <- eventReactive(input$btn_load, {
      if (is.null(input$metadata_file)) return(NULL)
      
      ext <- tools::file_ext(input$metadata_file$name)
      
      if (ext %in% c("xlsx", "xls")) {
        return(readxl::read_excel(input$metadata_file$datapath, sheet = 1))
      } else {
        return(read_emu_file(input$metadata_file))
      }
    })

    # Optional phylogenetic tree input
    loaded_tree <- eventReactive(input$btn_load, {
      req(input$tree_file)
      tryCatch({
        ape::read.tree(input$tree_file$datapath)
      }, error = function(e) {
        showNotification("Error reading the tree file. Make sure it is in the correct Newick format.", type = "error")
        return(NULL)
      })
    })
    
    output$tables_ui <- renderUI({
    # ==================================================
    # Previews and summaries
    # ==================================================

    # Show preview tabs only for the standard Emu workflow
      req(input$btn_load)
      if (input$analysis_type %in% c("16s", "its")) {
        tagList(
          hr(),
          tabsetPanel(
            tabPanel("Abundance", DTOutput(session$ns("abundance_tbl"))),
            tabPanel("Counts", DTOutput(session$ns("counts_tbl"))),
            tabPanel("Taxonomy", DTOutput(session$ns("taxonomy_tbl"))),
            tabPanel("Metadata", DTOutput(session$ns("metadata_tbl"))),
            tabPanel("Tree Info", verbatimTextOutput(session$ns("tree_summary")))
          )
        )
      }
    })
    
    # Tree summary preview
    output$tree_summary <- renderPrint({
      req(loaded_tree())
      cat("Phylogenetic tree loaded successfully!\n")
      print(loaded_tree())
    })
    
    # Data table previews
    output$abundance_tbl <- renderDT({ datatable(loaded_abundance(), options = list(scrollX = TRUE)) })
    output$counts_tbl    <- renderDT({ datatable(loaded_counts(), options = list(scrollX = TRUE)) })
    output$taxonomy_tbl  <- renderDT({ datatable(loaded_taxonomy(), options = list(scrollX = TRUE)) })
    output$metadata_tbl  <- renderDT({ 
      req(loaded_metadata())
      datatable(loaded_metadata(), options = list(scrollX = TRUE)) 
    })

    # ==================================================
    # Returned state for downstream modules
    # ==================================================
    return(list(
      type      = reactive(input$analysis_type),
      abundance = loaded_abundance,
      counts    = loaded_counts,
      taxonomy  = loaded_taxonomy,
      metadata  = loaded_metadata,
      tree      = loaded_tree  # phylo 
    ))
  })
}