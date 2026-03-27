# ============================================================
#  server.R — main server sceript
#  Navigation + module coordination via reactive contracts
# ============================================================

server <- function(input, output, session) {

  rv <- reactiveValues(
    page  = "upload",
    steps = c(upload = FALSE, qc = FALSE, filter = FALSE,
              annot  = FALSE, viz = FALSE)
  )

  nav_map <- list(
    nav_upload = list(page = "upload", pg_id = "pg_upload", label = "upload"),
    nav_qc     = list(page = "qc",     pg_id = "pg_qc",     label = "qc"),
    nav_filter = list(page = "filter", pg_id = "pg_filter", label = "filter"),
    nav_annot  = list(page = "annot",  pg_id = "pg_annot",  label = "annotate"),
    nav_viz    = list(page = "viz",    pg_id = "pg_viz",    label = "visualize")
  )

  lapply(names(nav_map), function(btn) {
    observeEvent(input[[btn]], {
      m <- nav_map[[btn]]
      rv$page <- m$page
      shinyjs::runjs(sprintf("
        document.querySelectorAll('.pg').forEach(p => p.classList.remove('active'));
        document.getElementById('%s').classList.add('active');
        document.querySelectorAll('.ri').forEach(b => b.classList.remove('active'));
        document.getElementById('%s').classList.add('active');
      ", m$pg_id, btn))
    })
  })

  output$strip_title <- renderText({
    nav_map[[paste0("nav_", rv$page)]]$label %||% rv$page
  })

  output$step_status <- renderUI({
    done  <- sum(rv$steps)
    total <- length(rv$steps)
    div(class = "step-counter", sprintf("%d/%d", done, total))
  })

  output$pipeline_dots <- renderUI({
    tagList(lapply(names(rv$steps), function(s) {
      cls <- paste("dot", if (rv$steps[[s]]) "done" else "")
      div(class = cls, title = s)
    }))
  })

  upload_out <- mod_upload_server("upload")
  qc_out <- mod_qc_server("qc", vcf = upload_out)
  filter_out <- mod_filter_server("filter", vcf = upload_out, qc = qc_out)
  annot_out <- mod_annotation_server("annot", filtered = filter_out)

  mod_visualization_server("viz",
    vcf      = upload_out,
    filtered = filter_out,
    annot    = annot_out
  )

  observe({ req(upload_out$vcf_data()); rv$steps["upload"] <- TRUE })
  observe({ req(qc_out$metrics());      rv$steps["qc"]     <- TRUE })
  observe({ req(filter_out$filtered()); rv$steps["filter"] <- TRUE })
  observe({ req(annot_out$annotated()); rv$steps["annot"]  <- TRUE })

  observeEvent(input$btn_run, {
    showNotification("Running current step…", type = "message", duration = 2)
  })
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

