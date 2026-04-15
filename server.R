server <- function(input, output, session) {
  
  #pratestuota, gauna duomenis
  
  # upload$data
  # upload$meta
  upload <- uploadServer("uploadID")
  
  #output$debug <- renderPrint({
  #  req(upload$abundance())
  #  upload$abundance()
  #})
  
  #filter paima data
  #filterServer("filterID", data = reactive(upload$data()))
  filter <- filterServer(
    "filterID",
    data          = reactive(upload$data()),
    abundance     = reactive(upload$abundance()),
    samples       = reactive(upload$samples()),
    metada        = reactive(upload$metadata()),
    analysis_type = reactive(upload$meta()$analysisType)
  )
  
  is_amplicon <- reactive({
    req(upload$meta())
    upload$meta()$analysisType %in% c("16s", "its")
  })
  
  analysis_data <- eventReactive(filter$btn_use(), {
    req(!is_amplicon(), filter$data())
    filter$data()
  })
  
  analysis_abundance <- eventReactive(filter$btn_use_ab(), {
    req(is_amplicon(), filter$abundance())
    filter$abundance()
  })
  
  analysis_samples <- eventReactive(filter$btn_use_smp(), {
    req(is_amplicon(), filter$samples())
    filter$samples()
  })
  
  amplicon <- ampliconServer(
    "ampliconID",
    abundance = reactive({
      # Naudojame isTruthy, kad saugiai patikrintume, ar mygtukas egzistuoja ir ar paspaustas
      btn_val <- filter$btn_use_ab()
      
      if (isTruthy(btn_val) && btn_val > 0) {
        # Svarbu: req() sustabdys vykdymą, jei analysis_abundance dar nesukurtas
        req(analysis_abundance())
        return(analysis_abundance())
      } else {
        # Jei mygtukas nepaspaustas, naudojame pradinius duomenis
        req(upload$abundance())
        return(upload$abundance())
      }
    }),
    samples = reactive({
      btn_val <- filter$btn_use_smp()
      
      if (isTruthy(btn_val) && btn_val > 0) {
        req(analysis_samples())
        return(analysis_samples())
      } else {
        req(upload$samples())
        return(upload$samples())
      }
    })
  )
  
  
  tbprofilerServer("tbID", files_info = upload$files, type = upload$type)
  
}