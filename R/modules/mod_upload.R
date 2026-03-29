# ============================================================
#  R/modules/mod_upload.R — Universal CSV/TSV Upload Module
#  Works with any tabular analysis output:
#  TB-Profiler, AMRFinder, MLST, Kraken, custom pipelines...
# ============================================================

mod_upload_ui <- function(id) {
  ns <- NS(id)

  page_wrap(
    "Upload",
    "Load any CSV or TSV results table — TB-Profiler, AMRFinder, MLST, or custom pipeline output.",

    two_col(

      # ── Left: file controls ────────────────────────────
      card(
        clabel("Input file"),
        div(class = "drop-zone",
          div(class = "drop-icon", "⬆"),
          div(class = "drop-label",
            tags$strong("Choose file"), " or drag & drop",
            tags$br(),
            tags$span(style = "font-size:10px",
                      ".csv  ·  .tsv  ·  .txt")
          )
        ),
        fileInput(ns("data_file"), NULL,
                  accept = c(".csv", ".tsv", ".txt",
                             "text/csv", "text/tab-separated-values")),

        clabel("Parse options"),
        selectInput(ns("sep"), "Delimiter",
                    choices = c(
                      "Auto-detect"  = "auto",
                      "Comma (CSV)"  = ",",
                      "Tab (TSV)"    = "\t",
                      "Semicolon"    = ";",
                      "Pipe"         = "|"
                    )),
        checkboxInput(ns("header"),   "First row is header", value = TRUE),
        checkboxInput(ns("stringsAsFactors"), "Strings as factors", value = FALSE),

        tags$hr(style = "border-color:#1c2433; margin: 14px 0"),

        clabel("Analysis type (optional label)"),
        selectInput(ns("analysis_type"), NULL,
                    choices = c(
                      "TB-Profiler"     = "tbprofiler",
                      "AMRFinder+"      = "amrfinder",
                      "MLST"            = "mlst",
                      "Kraken2"         = "kraken2",
                      "Custom / Other"  = "custom"
                    )),

        actionButton(ns("btn_load"), "load file", class = "action-btn")
      ),

      # ── Right: summary ─────────────────────────────────
      card(
        clabel("File summary"),
        kv_row(ns("out_filename"),  "File"),
        kv_row(ns("out_sep"),       "Delimiter"),
        kv_row(ns("out_filesize"),  "Size"),
        kv_row(ns("out_nrows"),     "Rows"),
        kv_row(ns("out_ncols"),     "Columns"),
        kv_row(ns("out_missing"),   "Missing values"),
        kv_row(ns("out_numcols"),   "Numeric columns"),
        kv_row(ns("out_type"),      "Analysis type")
      )
    ),

    # ── Column overview ───────────────────────────────────
    card_full(
      clabel("Column overview"),
      div(style = "overflow-x:auto",
        uiOutput(ns("col_pills"))
      )
    ),

    # ── Data preview ──────────────────────────────────────
    card_full(
      clabel("Data preview"),
      DTOutput(ns("preview_tbl"))
    )
  )
}

# ─────────────────────────────────────────────────────────────

mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ── Auto-detect delimiter ─────────────────────────────
    detect_sep <- function(path) {
      first <- readLines(path, n = 5, warn = FALSE)
      line  <- first[1]
      counts <- c(
        ","  = nchar(line) - nchar(gsub(",",  "", line, fixed = TRUE)),
        "\t" = nchar(line) - nchar(gsub("\t", "", line, fixed = TRUE)),
        ";"  = nchar(line) - nchar(gsub(";",  "", line, fixed = TRUE)),
        "|"  = nchar(line) - nchar(gsub("|",  "", line, fixed = TRUE))
      )
      names(which.max(counts))
    }

    # ── Reactive: parsed data ─────────────────────────────
    table_data <- eventReactive(input$btn_load, {
      req(input$data_file)
      path <- input$data_file$datapath

      sep <- if (input$sep == "auto") detect_sep(path) else input$sep

      tryCatch(
        read.delim(path,
                   sep              = sep,
                   header           = input$header,
                   stringsAsFactors = input$stringsAsFactors,
                   check.names      = FALSE,
                   encoding         = "UTF-8"),
        error = function(e) {
          showNotification(paste("Parse error:", e$message),
                           type = "error", duration = 8)
          NULL
        }
      )
    })

    # ── Detected sep label ────────────────────────────────
    sep_label <- eventReactive(input$btn_load, {
      req(input$data_file)
      if (input$sep == "auto") {
        s <- detect_sep(input$data_file$datapath)
        switch(s, "," = "comma (auto)", "\t" = "tab (auto)",
               ";" = "semicolon (auto)", "|" = "pipe (auto)", s)
      } else {
        switch(input$sep, "," = "comma", "\t" = "tab",
               ";" = "semicolon", "|" = "pipe", input$sep)
      }
    })

    # ── Summary outputs ───────────────────────────────────
    output$out_filename <- renderText({
      req(input$data_file); input$data_file$name
    })
    output$out_sep      <- renderText({ req(sep_label()); sep_label() })
    output$out_filesize <- renderText({
      req(input$data_file)
      sz <- file.info(input$data_file$datapath)$size
      if (sz > 1e6) paste0(round(sz/1e6,  2), " MB")
      else          paste0(round(sz/1024,  1), " KB")
    })
    output$out_nrows    <- renderText({
      req(table_data()); format(nrow(table_data()), big.mark = ",")
    })
    output$out_ncols    <- renderText({
      req(table_data()); ncol(table_data())
    })
    output$out_missing  <- renderText({
      req(table_data())
      n <- sum(is.na(table_data()))
      pct <- round(n / (nrow(table_data()) * ncol(table_data())) * 100, 1)
      paste0(format(n, big.mark=","), "  (", pct, "%)")
    })
    output$out_numcols  <- renderText({
      req(table_data())
      sum(sapply(table_data(), is.numeric))
    })
    output$out_type     <- renderText({ input$analysis_type })

    # ── Column pills ──────────────────────────────────────
    output$col_pills <- renderUI({
      req(table_data())
      df <- table_data()
      pills <- lapply(names(df), function(col) {
        cls <- if (is.numeric(df[[col]])) "pill numeric" else "pill text"
        div(class = cls,
          tags$span(class = "pill-name", col),
          tags$span(class = "pill-type",
                    if (is.numeric(df[[col]])) "num" else "chr")
        )
      })
      tagList(
        tags$style(HTML("
          .pill-row { display:flex; flex-wrap:wrap; gap:6px; margin-top:4px; }
          .pill { display:inline-flex; align-items:center; gap:5px;
                  border-radius:4px; padding:4px 9px; font-size:11px;
                  border:1px solid var(--border-hi); }
          .pill.numeric { background:rgba(91,184,255,.08); border-color:rgba(91,184,255,.3); }
          .pill.text    { background:rgba(61,255,160,.06); border-color:rgba(61,255,160,.2); }
          .pill-name { color:var(--txt); }
          .pill-type { color:var(--txt-faint); font-size:9px; font-style:italic; }
        ")),
        div(class = "pill-row", pills)
      )
    })

    # ── Preview table ──────────────────────────────────────
    output$preview_tbl <- renderDT({
      req(table_data())
      datatable(
        head(table_data(), 500),
        options  = list(
          scrollX    = TRUE,
          pageLength = 15,
          dom        = "ftip"
        ),
        class    = "display compact",
        rownames = FALSE,
        filter   = "top"
      )
    })

    # ── Contract for downstream modules ───────────────────
    list(
      data          = table_data,
      filename      = reactive(input$data_file$name),
      analysis_type = reactive(input$analysis_type),
      sep           = sep_label
    )
  })
}
