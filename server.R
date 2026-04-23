server <- function(input, output, session) {
  upload <- uploadServer("uploadID")
  
  ab_filtered     <- filterServer("ab_filt", original_data = reactive(upload$abundance()), tax_data = reactive(upload$taxonomy()))
  counts_filtered <- filterServer("counts_filt", original_data = reactive(upload$counts()), tax_data = reactive(upload$taxonomy()))
  tax_filtered    <- filterServer("tax_filt", original_data = reactive(upload$taxonomy()))
  meta_filtered   <- metadataFilterServer("meta_filt", reactive(upload$metadata()))
  
  confirmed_meta <- reactiveVal(NULL)
  confirmed_abundance <- reactiveVal(NULL)
  confirmed_counts <- reactiveVal(NULL)
  confirmed_taxonomy <- reactiveVal(NULL)
  
  observeEvent(meta_filtered$btn_use(), {
    req(meta_filtered$btn_use() > 0)
    confirmed_meta(meta_filtered$data())

    samples_now <- rownames(meta_filtered$data())
    if (is.null(samples_now) || all(samples_now == as.character(seq_len(nrow(meta_filtered$data()))))) {
      samples_now <- meta_filtered$data()[[1]]
    }
    
    # Abundance
    ab_df <- if (isTruthy(ab_filtered$btn_use()) && ab_filtered$btn_use() > 0) {
      ab_filtered$data()
    } else {
      upload$abundance()
    }
    if (!is.null(ab_df)) {
      non_sample_cols <- names(ab_df)[!sapply(ab_df, is.numeric) | names(ab_df) == "tax_id"]
      sample_cols_keep <- intersect(colnames(ab_df), samples_now)
      confirmed_abundance(ab_df[, c(non_sample_cols, sample_cols_keep), drop = FALSE])
    }
    
    # Counts
    cnt_df <- if (isTruthy(counts_filtered$btn_use()) && counts_filtered$btn_use() > 0) {
      counts_filtered$data()
    } else {
      upload$counts()
    }
    if (!is.null(cnt_df)) {
      non_sample_cols <- names(cnt_df)[!sapply(cnt_df, is.numeric) | names(cnt_df) == "tax_id"]
      sample_cols_keep <- intersect(colnames(cnt_df), samples_now)
      confirmed_counts(cnt_df[, c(non_sample_cols, sample_cols_keep), drop = FALSE])
    }
    
    showNotification("NEW metadata", type = "message", duration = 3)
  })

  final_metadata <- reactive({
    confirmed_meta() %||% upload$metadata()
  })
  
  final_abundance <- reactive({
    df <- confirmed_abundance() %||% {
      if (isTruthy(ab_filtered$btn_use()) && ab_filtered$btn_use() > 0)
        ab_filtered$data()
      else
        upload$abundance()
    }
    req(df)
    df
  })
  
  final_counts <- reactive({
    df <- confirmed_counts() %||% {
      if (isTruthy(counts_filtered$btn_use()) && counts_filtered$btn_use() > 0)
        counts_filtered$data()
      else
        upload$counts()
    }
    req(df)
    df
  })
  
  final_taxonomy <- reactive({
    confirmed_taxonomy() %||% {
      if (isTruthy(tax_filtered$btn_use()) && tax_filtered$btn_use() > 0)
        tax_filtered$data()
      else
        upload$taxonomy()
    }
  })
  
  analysis_all <- list(
    abundance = final_abundance,
    counts    = final_counts,
    taxonomy  = final_taxonomy,
    meta      = final_metadata,
    tree      = upload$tree
  )
  
  ampliconServer("amplicon_1", analysis_all)
}