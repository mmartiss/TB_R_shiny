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