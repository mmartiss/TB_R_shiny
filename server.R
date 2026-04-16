server <- function(input, output, session) {
  
  #pratestuota, gauna duomenis
  
  # upload$data
  # upload$meta
  upload <- uploadServer("uploadID")
  
  ab_filtered <- filterServer("ab_filt", 
                              original_data = reactive(upload$abundance()), 
                              tax_data = reactive(upload$taxonomy()))
  
  counts_filtered <- filterServer("counts_filt", 
                                  original_data = reactive(upload$counts()), 
                                  tax_data = reactive(upload$taxonomy()))
  
  tax_filtered <- filterServer("tax_filt", original_data = reactive(upload$taxonomy()))
  
  meta_filtered   <- metadataFilterServer("meta_filt", reactive(upload$metadata()))
  
  # pasileidzia, jei buvo paspaustas mygtukas "use for analysis"
  analysis_abundance <- eventReactive(ab_filtered$btn_use(), {
    req(ab_filtered$data())
    ab_filtered$data()
  })
  
  analysis_counts <- eventReactive(counts_filtered$btn_use(), {
    req(counts_filtered$data())
    counts_filtered$data()
  })
  
  analysis_taxonomy <- eventReactive(tax_filtered$btn_use(), {
    req(tax_filtered$data())
    tax_filtered$data()
  })

  analysis_metadata <- eventReactive(meta_filtered$btn_use(), {
    req(meta_filtered$data())
    meta_filtered$data()
  })
  
  final_abundance <- reactive({
    if (isTruthy(ab_filtered$btn_use()) && ab_filtered$btn_use() > 0) {
      req(analysis_abundance())
    } else {
      req(upload$abundance())
    }
  })
  
  final_counts <- reactive({
    if (isTruthy(counts_filtered$btn_use()) && counts_filtered$btn_use() > 0) {
      req(analysis_counts())
    } else {
      req(upload$counts())
    }
  })
  
  final_taxonomy <- reactive({
    if (isTruthy(tax_filtered$btn_use()) && tax_filtered$btn_use() > 0) {
      req(analysis_taxonomy())
    } else {
      req(upload$taxonomy())
    }
  })
  
  final_metadata <- reactive({
    if (isTruthy(meta_filtered$btn_use()) && meta_filtered$btn_use() > 0) {
      req(analysis_metadata())
    } else {
      req(upload$metadata())
    }
  })
  
  
  analysis_all <- list(
    abundance = final_abundance,
    counts = final_counts,
    taxonomy = final_taxonomy,
    meta = final_metadata,
    tree = upload$tree
  )
  
  ampliconServer("amplicon_1", analysis_all)
  
  
  #output$debug <- renderPrint({
  #  req(upload$abundance())
  #  upload$abundance()
  #})
  
  #filter paima data
  #filterServer("filterID", data = reactive(upload$data()))
  # filter <- filterServer(
  #   "filterID",
  #   data          = reactive(upload$data()),
  #   abundance     = reactive(upload$abundance()),
  #   samples       = reactive(upload$samples()),
  #   metada        = reactive(upload$metadata()),
  #   analysis_type = reactive(upload$meta()$analysisType)
  # )
  
  # is_amplicon <- reactive({
  #   req(upload$meta())
  #   upload$meta()$analysisType %in% c("16s", "its")
  # })
  # 
  # amplicon <- ampliconServer(
  #   "ampliconID",
  #   abundance = reactive({
  #     # Naudojame isTruthy, kad saugiai patikrintume, ar mygtukas egzistuoja ir ar paspaustas
  #     btn_val <- filter$btn_use_ab()
  #     
  #     if (isTruthy(btn_val) && btn_val > 0) {
  #       # Svarbu: req() sustabdys vykdymą, jei analysis_abundance dar nesukurtas
  #       req(analysis_abundance())
  #       return(analysis_abundance())
  #     } else {
  #       # Jei mygtukas nepaspaustas, naudojame pradinius duomenis
  #       req(upload$abundance())
  #       return(upload$abundance())
  #     }
  #   }),
  #   samples = reactive({
  #     btn_val <- filter$btn_use_smp()
  #     
  #     if (isTruthy(btn_val) && btn_val > 0) {
  #       req(analysis_samples())
  #       return(analysis_samples())
  #     } else {
  #       req(upload$samples())
  #       return(upload$samples())
  #     }
  #   })
  # )
  
}