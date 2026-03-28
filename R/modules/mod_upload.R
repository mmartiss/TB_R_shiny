# ============================================================
#  R/modules/mod_upload.R — VCF Upload Module
# ============================================================

mod_upload_ui <- function(id) {
  ns <- NS(id)
  page_wrap("Upload",
    "Load a VCF / gVCF file. Multi-sample and bgzipped inputs supported.",

    two_col(
      card(
        card_label("Input file"),
        fileInput(ns("vcf_file"), NULL,
                  accept = c(".vcf", ".vcf.gz", ".bcf"),
                  placeholder = "sample.vcf / .vcf.gz"),
        card_label("Reference genome"),
        selectInput(ns("genome"), NULL,
                    choices = c("GRCh38 / hg38" = "hg38",
                                "GRCh37 / hg19" = "hg19",
                                "GRCm39 / mm39"  = "mm39")),
        checkboxInput(ns("multiallelic_split"),
                      "Split multi-allelic sites", value = TRUE),
        checkboxInput(ns("pass_only"),
                      "Load FILTER=PASS only",     value = FALSE)
      ),
      card(
        card_label("File summary"),
        kv_row(ns("n_variants"), "Total variants"),
        kv_row(ns("n_samples"),  "Samples"),
        kv_row(ns("n_chroms"),   "Chromosomes"),
        kv_row(ns("vcf_format"), "Format"),
        kv_row(ns("file_size"),  "File size")
      )
    ),

    card_full(
      card_label("Variant preview — first 200 rows"),
      DTOutput(ns("preview"))
    )
  )
}

mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    vcf_data <- reactive({
      req(input$vcf_file)

      # ── TODO: replace with real VCF parser ─────────────
      message("mod_upload: parsing ", input$vcf_file$name)

      data.frame(
        CHROM  = character(0), POS    = integer(0),
        ID     = character(0), REF    = character(0),
        ALT    = character(0), QUAL   = numeric(0),
        FILTER = character(0), INFO   = character(0),
        stringsAsFactors = FALSE
      )
    })

    vcf_header <- reactive({
      req(input$vcf_file)
      # TODO: VariantAnnotation::scanVcfHeader(input$vcf_file$datapath)
      list(genome = input$genome, source = input$vcf_file$name)
    })

    samples <- reactive({
      req(vcf_data())
      # TODO: extract sample names from VCF header
      character(0)
    })

    output$n_variants <- renderText({ req(vcf_data()); nrow(vcf_data()) })
    output$n_samples  <- renderText({ req(samples());  length(samples()) })
    output$n_chroms   <- renderText({
      req(vcf_data())
      if (nrow(vcf_data()) > 0) length(unique(vcf_data()$CHROM)) else 0
    })
    output$vcf_format <- renderText({ req(input$vcf_file); "VCF 4.2" })
    output$file_size  <- renderText({
      req(input$vcf_file)
      paste0(round(file.info(input$vcf_file$datapath)$size / 1024, 1), " KB")
    })

    output$preview <- renderDT({
      req(vcf_data())
      datatable(head(vcf_data(), 200),
                options = list(scrollX = TRUE, pageLength = 10,
                               dom = "tip"),
                class   = "display compact")
    })

    list(
      vcf_data   = vcf_data,
      vcf_header = vcf_header,
      samples    = samples
    )
  })
}
