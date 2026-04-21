server <- function(input, output, session) {
  upload <- uploadServer("uploadID")
  
  ab_filtered <- filterServer("ab_filt", 
                              original_data = reactive(upload$abundance()), 
                              tax_data = reactive(upload$taxonomy()))
  
  counts_filtered <- filterServer("counts_filt", 
                                  original_data = reactive(upload$counts()), 
                                  tax_data = reactive(upload$taxonomy()))
  
  tax_filtered <- filterServer("tax_filt", original_data = reactive(upload$taxonomy()))
  
  meta_filtered   <- metadataFilterServer("meta_filt", reactive(upload$metadata()))

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
}