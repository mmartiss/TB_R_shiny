server <- function(input, output, session) {
  upload <- uploadServer("uploadID")
  
  active_data <- reactiveValues(
    abundance = NULL,
    counts = NULL,
    taxonomy = NULL,
    metadata = NULL
  )
  
  ab_filtered     <- filterServer("ab_filt", 
                                  original_data = reactive(upload$abundance()), 
                                  tax_data = reactive(upload$taxonomy()))
  
  counts_filtered <- filterServer("counts_filt", 
                                  original_data = reactive(upload$counts()), 
                                  tax_data = reactive(upload$taxonomy()))
  
  tax_filtered    <- filterServer("tax_filt", 
                                  original_data = reactive(upload$taxonomy()))
  
  meta_filtered   <- metadataFilterServer("meta_filt", 
                                          reactive(upload$metadata()))
  
  observeEvent(upload$abundance(), { active_data$abundance <- upload$abundance() })
  observeEvent(upload$counts(),    { active_data$counts    <- upload$counts()    })
  observeEvent(upload$taxonomy(),  { active_data$taxonomy  <- upload$taxonomy()  })
  observeEvent(upload$metadata(),  { active_data$metadata  <- upload$metadata()  })
  
  observeEvent(ab_filtered$btn_use(), {
    req(ab_filtered$data())
    active_data$abundance <- ab_filtered$data()
  })
  
  observeEvent(counts_filtered$btn_use(), {
    req(counts_filtered$data())
    active_data$counts <- counts_filtered$data()
  })
  
  observeEvent(tax_filtered$btn_use(), {
    req(tax_filtered$data())
    active_data$taxonomy <- tax_filtered$data()
  })
  
  observeEvent(meta_filtered$btn_use(), {
    req(meta_filtered$data())
    active_data$metadata <- meta_filtered$data()
  })
  
  analysis_all <- list(
    abundance = reactive({ active_data$abundance }),
    counts    = reactive({ active_data$counts    }),
    taxonomy  = reactive({ active_data$taxonomy  }),
    meta      = reactive({ active_data$metadata  }),
    tree      = upload$tree
  )
  
  ampliconServer("amplicon_1", analysis_all)
  
  session$onSessionEnded(function() {
    stopApp()
    q("no")
  })
}