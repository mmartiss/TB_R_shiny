server <- function(input, output, session) {
  
  #pratestuota, gauna duomenis
  
  # upload$data
  # upload$meta
  upload <- uploadServer("uploadID")

  #filter paima data
  #filterServer("filterID", data = reactive(upload$data()))
  filter <- filterServer("filterID", data = reactive(upload$data()))
  analysis_data <- eventReactive(filter$btn_use(), {
    req(filter$data())
    filter$data()
  })
  
  
  
  output$debug <- renderPrint({
    req(upload$meta())
    upload$meta()
  })
  
}