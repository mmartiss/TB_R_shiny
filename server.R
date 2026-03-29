# ============================================================
#  server.R
# ============================================================

server <- function(input, output, session) {

  rv <- reactiveValues(
    page  = "upload",
    steps = c(upload = FALSE)
  )

  observeEvent(input$nav_upload, { rv$page <- "upload" })

  output$strip_title <- renderText({ rv$page })

  output$step_status <- renderUI({
    done  <- sum(rv$steps)
    total <- length(rv$steps)
    div(class = "step-counter", sprintf("%d / %d", done, total))
  })

  output$pipeline_dots <- renderUI({
    tagList(lapply(names(rv$steps), function(s) {
      div(class = paste("dot", if (isTRUE(rv$steps[[s]])) "done"),
          title = s)
    }))
  })

  # Upload module
  upload_out <- mod_upload_server("upload")
  #upload_out$data          : reactive data.frame
  #upload_out$filename      : reactive character
  #upload_out$analysis_type : reactive character
  #upload_out$sep           : reactive character

  observe({
    req(upload_out$data())
    rv$steps["upload"] <- TRUE
  })
}
