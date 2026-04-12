server <- function(input, output, session) {
  
  #pratestuota, gauna duomenis
  
  # upload$data
  # upload$meta
  upload <- uploadServer("uploadID")
  
  output$debug <- renderPrint({
    req(upload$abundance())
    upload$abundance()
  })
  
  #filter paima data
  #filterServer("filterID", data = reactive(upload$data()))
  filter <- filterServer(
    "filterID",
    data          = reactive(upload$data()),
    abundance     = reactive(upload$abundance()),
    samples       = reactive(upload$samples()),
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
      if (isTruthy(analysis_abundance())) analysis_abundance() else upload$abundance()
    }),
    samples = reactive({
      if (isTruthy(analysis_samples())) analysis_samples() else upload$samples()
    })
  )
  

  
}