# server.R

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    page  = "upload",
    # Track which steps are complete
    steps = c(upload = FALSE, qc = FALSE, filter = FALSE, annot = FALSE, viz = FALSE)
  )
  
  # --- Navigation Logic ---
  # This observer switches classes to show/hide pages when buttons are clicked
  observe({
    req(input$nav_upload, input$nav_qc, input$nav_filter, input$nav_annot, input$nav_viz)
    
    pages <- c("upload", "qc", "filter", "annot", "viz")
    
    lapply(pages, function(p) {
      observeEvent(input[[paste0("nav_", p)]], {
        rv$page <- p
        # Use shinyjs to toggle visibility via the "active" class defined in your CSS
        for (pg in pages) {
          if (pg == p) {
            shinyjs::addClass(selector = paste0("#pg_", pg), class = "active")
            shinyjs::addClass(id = paste0("nav_", pg), class = "active")
          } else {
            shinyjs::removeClass(selector = paste0("#pg_", pg), class = "active")
            shinyjs::removeClass(id = paste0("nav_", pg), class = "active")
          }
        }
      })
    })
  })
  
  output$strip_title <- renderText({ rv$page })

  upload_out <- mod_upload_server("upload")

  qc_out <- mod_qc_server("qc", data = upload_out$data)

  filter_out <- mod_filter_server("filter", data = upload_out$data)

  annot_out <- mod_annotation_server("annot", data = filter_out$data)

  mod_visualization_server("viz", data = filter_out$data)
  
  observe({
    req(upload_out$data())
    rv$steps["upload"] <- TRUE
  })
  
  output$step_status <- renderUI({
    done  <- sum(rv$steps)
    total <- length(rv$steps)
    div(class = "step-counter", sprintf("%d / %d", done, total))
  })
  
  output$pipeline_dots <- renderUI({
    tagList(lapply(names(rv$steps), function(s) {
      div(class = paste("dot", if (isTRUE(rv$steps[[s]])) "done"), title = s)
    }))
  })
}