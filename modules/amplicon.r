# Reikalingos bibliotekos: shiny, dplyr, tidyr, plotly, vegan

ampliconUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Amplicon Analysis"),
    fluidRow(
      column(3,
             tags$label("Taxonomic level"),
             selectInput(ns("tax_level"), NULL,
                         choices  = c("species", "genus", "family", "order", "class", "phylum"),
                         selected = "genus")
      ),
      column(2,
             tags$label("Top N taxa"),
             numericInput(ns("top_n"), NULL, value = 20, min = 5, max = 100, step = 5)
      ),
      column(2, 
             tags$label("Alpha Metric"),
             selectInput(ns("alpha_metric"), NULL, 
                         choices = c("Shannon", "Simpson", "Observed"), 
                         selected = "Shannon")
      ),
      column(2,
             tags$label("Color palette"),
             selectInput(ns("color_palette"), NULL,
                         choices = c("Tableau 10", "Pastel", "Bold", "Viridis-disc", "Earth"),
                         selected = "Tableau 10")
      ),
      column(3,
             tags$br(),
             actionButton(ns("btn_generate"), "Update Analysis", class = "btn-success w-100")
      )
    ),
    hr(),
    tabsetPanel(
      tabPanel("Taxonomic Bar Chart", 
               uiOutput(ns("color_editor_ui")),
               plotlyOutput(ns("plot_bar"), height = "600px")),
      
      tabPanel("Heatmap", 
               plotlyOutput(ns("plot_heatmap"), height = "600px")),
      
      # tabPanel("Alpha Diversity", 
      #          plotlyOutput(ns("plot_alpha"), height = "500px")),
      
      tabPanel("Beta Diversity (PCoA)", 
               plotlyOutput(ns("plot_beta"), height = "600px"))
    ),
    tabPanel("Alpha Diversity", 
             fluidRow(
               column(12,
                      wellPanel(
                        fluidRow(
                          column(6,
                                 checkboxGroupInput(
                                   ns("alpha_metrics_sel"), 
                                   "Select Metrics:",
                                   choices = c(
                                     "Observed (Unique)" = "Observed", 
                                     "Shannon" = "Shannon", 
                                     "Simpson" = "Simpson", 
                                     "InvSimpson" = "InvSimpson"
                                   ),
                                   selected = c("Observed", "Shannon")
                                 )
                          ),
                          column(6,
                                 div(
                                   style = "padding-top: 25px;",
                                   helpText("The boxplot shows the distribution of all samples, while the dots show specific samples.")
                                 )
                          )
                        )
                      )
               )
             ),
             
             fluidRow(
               column(12,
                      plotlyOutput(ns("plot_alpha_multi"), height = "600px")
               )
             )
    )
  )
}

ampliconServer <- function(id, data_res) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    PALETTES <- list(
      "Tableau 10"   = c("#4E79A7","#F28E2B","#E15759","#76B7B2","#59A14F","#EDC948","#B07AA1","#FF9DA7","#9C755F","#BAB0AC"),
      "Pastel"       = c("#AEC6CF","#FFD1DC","#B5EAD7","#FFDAC1","#C7CEEA","#E2F0CB","#F2D7D9","#D5E8D4","#DAE8FC","#F8CECC"),
      "Bold"         = c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#A65628","#F781BF","#999999","#66C2A5","#FC8D62"),
      "Viridis-disc" = c("#440154","#30678D","#35B779","#FDE724","#482677","#1F9E89","#73D055","#DCE319","#3CBB75","#94D840"),
      "Earth"        = c("#8C510A","#BF812D","#DFC27D","#80CDC1","#35978F","#01665E","#543005","#F6E8C3","#C7EAE5","#003C30")
    )
    
    rv_colors <- reactiveValues(map = list())
    
    # --- PAGRINDINIS DUOMENŲ APDOROJIMAS ---
    processed_counts <- eventReactive(input$btn_generate, {
      req(data_res$abundance, data_res$taxonomy)
      
      ab   <- data_res$abundance()
      tax  <- data_res$taxonomy()
      level <- input$tax_level
      
      # 1. Identifikuojame mėginių stulpelius
      all_cols <- names(ab)
      sample_cols <- all_cols[sapply(ab, is.numeric) & all_cols != "tax_id"]
      
      # 2. Jungiame su taksonomija
      ab$tax_id <- as.character(ab$tax_id)
      tax$tax_id <- as.character(tax$tax_id)
      merged <- dplyr::inner_join(ab, tax, by = "tax_id")
      
      # 3. Sutvarkome Unknowns
      merged[[level]][is.na(merged[[level]]) | merged[[level]] == "" | merged[[level]] == "unclassified"] <- "Unknown"
      
      # 4. Agreguojame counts
      long_counts <- merged %>%
        dplyr::select(dplyr::all_of(c(level, sample_cols))) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(sample_cols), names_to = "sample", values_to = "counts") %>%
        dplyr::mutate(sample = gsub("\\.fastq$|\\.fq$|\\.fastq\\.gz$|\\.fastq-threshold-.*$", "", sample)) %>%
        dplyr::group_by(sample, taxon = .data[[level]]) %>%
        dplyr::summarise(counts = sum(counts, na.rm = TRUE), .groups = "drop")
      
      # --- FILTRAS: Pašaliname mėginius, kurie neturi jokių counts ---
      valid_samples <- long_counts %>%
        dplyr::group_by(sample) %>%
        dplyr::summarise(total = sum(counts)) %>%
        dplyr::filter(total > 0) %>%
        dplyr::pull(sample)
      
      long_counts <- long_counts %>% dplyr::filter(sample %in% valid_samples)
      
      # 5. Paruošiame matricą vegan paketui
      matrix_counts <- long_counts %>%
        tidyr::pivot_wider(names_from = taxon, values_from = counts, values_fill = 0) %>%
        tibble::column_to_rownames("sample")
      
      list(long = long_counts, matrix = matrix_counts)
    })
    
    # Duomenys bar chart'ui (su "Other" grupavimu)
    prepared_bar_data <- reactive({
      req(processed_counts())
      ld <- processed_counts()$long
      
      top_taxa <- ld %>%
        dplyr::group_by(taxon) %>%
        dplyr::summarise(mean_val = mean(counts)) %>%
        dplyr::filter(taxon != "Unknown") %>%
        dplyr::arrange(desc(mean_val)) %>%
        dplyr::slice_head(n = input$top_n) %>%
        dplyr::pull(taxon)
      
      final_df <- ld %>%
        dplyr::mutate(taxon = ifelse(taxon %in% top_taxa, taxon, "Other")) %>%
        dplyr::group_by(sample, taxon) %>%
        dplyr::summarise(counts = sum(counts), .groups = "drop") %>%
        dplyr::group_by(sample) %>%
        dplyr::mutate(rel_abund = counts / sum(counts)) %>%
        dplyr::ungroup()
      
      final_df
    })
    
    # --- SPALVŲ VALDYMAS ---
    observe({
      req(prepared_bar_data())
      taxa <- unique(prepared_bar_data()$taxon)
      pal  <- PALETTES[[input$color_palette]]
      cols <- rep_len(pal, length(taxa))
      names(cols) <- taxa
      if("Other" %in% names(cols)) cols["Other"] <- "#D3D3D3"
      if("Unknown" %in% names(cols)) cols["Unknown"] <- "#A9A9A9"
      rv_colors$map <- as.list(cols)
    })
    
    output$color_editor_ui <- renderUI({
      req(rv_colors$map)
      taxa <- names(rv_colors$map)
      tags$div(style = "display:flex; flex-wrap:wrap; gap:10px; padding: 10px; background: #f9f9f9; border-bottom: 1px solid #ddd;",
               lapply(taxa, function(t) {
                 input_id <- paste0("col_", gsub("[^A-Za-z0-9]", "_", t))
                 tags$div(style = "display:flex; align-items:center; gap:5px;",
                          tags$input(type = "color", value = rv_colors$map[[t]],
                                     onchange = sprintf("Shiny.setInputValue('%s', this.value)", ns(input_id)),
                                     style="width:20px; height:20px; border:none;"),
                          tags$span(t, style="font-size:10px;")
                 )
               })
      )
    })
    
    # extra alpha
    
    # 1. Alpha diversity skaičiavimas
    alpha_data <- reactive({
      req(processed_counts())
      mat <- processed_counts()$matrix
      
      # Skaičiuojame indeksus
      df <- data.frame(
        Sample = rownames(mat),
        Observed = rowSums(mat > 0),
        Shannon = vegan::diversity(mat, index = "shannon"),
        Simpson = vegan::diversity(mat, index = "simpson"),
        InvSimpson = vegan::diversity(mat, index = "invsimpson"),
        check.names = FALSE
      )
      
      df %>% tidyr::pivot_longer(cols = -Sample, names_to = "Metric", values_to = "Value")
    })
    
    # 2. Alpha diversity grafikas
    output$plot_alpha_multi <- renderPlotly({
      req(alpha_data(), input$alpha_metrics_sel)
      
      df_filtered <- alpha_data() %>%
        dplyr::filter(Metric %in% input$alpha_metrics_sel) %>%
        # Pašaliname bet kokius NaN/Inf, jei tokių liko
        dplyr::filter(!is.na(Value) & !is.infinite(Value))
      
      if(nrow(df_filtered) == 0) return(NULL)
      
      p <- suppressWarnings({
        ggplot2::ggplot(df_filtered, ggplot2::aes(x = Metric, y = Value, fill = Metric)) +
          ggplot2::geom_boxplot(alpha = 0.6, outlier.shape = NA) +
          ggplot2::geom_jitter(ggplot2::aes(text = paste("Sample:", Sample, "<br>Value:", round(Value, 3))), 
                               width = 0.15, size = 2, alpha = 0.7) +
          ggplot2::facet_wrap(~Metric, scales = "free", nrow = 1) + 
          ggplot2::theme_minimal() +
          ggplot2::theme(
            legend.position = "none",
            panel.spacing = ggplot2::unit(1, "lines"), # Kiek padidinau tarpą
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill = "#f0f0f0", color = NA),
            strip.text = ggplot2::element_text(face = "bold")
          ) +
          ggplot2::scale_fill_brewer(palette = "Set2")
      })
      
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(margin = list(t = 40, b = 20, l = 40, r = 10))
    })
    
    # --- ALPHA DIVERSITY ---
    # output$plot_alpha <- renderPlotly({
    #   req(processed_counts())
    #   mat <- processed_counts()$matrix
    #   
    #   alpha_df <- data.frame(sample = rownames(mat))
    #   
    #   if (input$alpha_metric == "Shannon") {
    #     alpha_df$value <- vegan::diversity(mat, index = "shannon")
    #   } else if (input$alpha_metric == "Simpson") {
    #     alpha_df$value <- vegan::diversity(mat, index = "simpson")
    #   } else {
    #     alpha_df$value <- rowSums(mat > 0)
    #   }
    #   
    #   plotly::plot_ly(alpha_df, x = ~sample, y = ~value, type = "bar", marker = list(color = "#4E79A7")) %>%
    #     plotly::layout(title = paste(input$alpha_metric, "Diversity Index"),
    #                    xaxis = list(title = "Sample", tickangle = -45),
    #                    yaxis = list(title = "Index Value"))
    # })
    
    # --- BETA DIVERSITY (PCoA) ---
    output$plot_beta <- renderPlotly({
      req(processed_counts())
      mat <- processed_counts()$matrix
      
      # Skaičiuojame atstumus ir PCoA
      dist_mat <- vegan::vegdist(mat, method = "bray")
      pcoa_res <- cmdscale(dist_mat, k = 2, eig = TRUE)
      
      pcoa_df <- data.frame(
        sample = rownames(mat),
        PC1 = pcoa_res$points[,1],
        PC2 = pcoa_res$points[,2]
      )
      
      var_exp <- round(pcoa_res$eig / sum(pcoa_res$eig) * 100, 1)
      
      plotly::plot_ly(pcoa_df, 
                      x = ~PC1, 
                      y = ~PC2, 
                      # Suformuojame tekstą, kurį matysime užvedę pelę
                      text = ~paste0("<b>Sample:</b> ", sample, 
                                     "<br><b>PC1:</b> ", round(PC1, 3),
                                     "<br><b>PC2:</b> ", round(PC2, 3)),
                      type = "scatter", 
                      mode = "markers",      # PAKEISTA: tik taškai, be teksto grafike
                      hoverinfo = "text",    # PAKEISTA: naudoti tik 'text' lauko informaciją
                      marker = list(size = 12, 
                                    color = "#E15759",
                                    line = list(color = "white", width = 1))) %>%
        plotly::layout(title = "Beta Diversity (PCoA - Bray Curtis)",
                       xaxis = list(title = paste0("PC1 (", var_exp[1], "%)")),
                       yaxis = list(title = paste0("PC2 (", var_exp[2], "%)")),
                       hovermode = "closest") # Užtikrina geresnį pelės sekimą
    })
    
    # --- SENI GRAFIKAI (Bar & Heatmap) ---
    output$plot_bar <- renderPlotly({
      req(prepared_bar_data(), rv_colors$map)
      df <- prepared_bar_data()
      cols <- unlist(rv_colors$map)
      plotly::plot_ly(df, x = ~sample, y = ~rel_abund, color = ~taxon, colors = cols,
                      type = "bar", hoverinfo = "text",
                      text = ~paste0(taxon, ": ", round(rel_abund*100, 2), "%")) %>%
        plotly::layout(barmode = "stack", yaxis = list(tickformat = ".0%"), xaxis = list(tickangle = -45))
    })
    
    output$plot_heatmap <- renderPlotly({
      req(processed_counts())
      # Naudojame originalią matricą ir paverčiame į santykinę gausą
      mat <- as.matrix(processed_counts()$matrix)
      # Normalizuojame eilutes (mėginius), kad suma būtų 1
      mat_rel <- sweep(mat, 1, rowSums(mat), "/")
      
      # Atrankome tik Top N taksonus, kad heatmap nebūtų per didelis
      top_taxa_names <- names(sort(colMeans(mat_rel), decreasing = TRUE))[1:min(input$top_n, ncol(mat_rel))]
      mat_plot <- t(mat_rel[, top_taxa_names, drop = FALSE]) # Transponuojame: Taxa = rows, Samples = cols
      
      plotly::plot_ly(
        x = colnames(mat_plot), 
        y = rownames(mat_plot), 
        z = mat_plot, 
        type = "heatmap", 
        colorscale = "Viridis",
        reversescale = FALSE
      ) %>%
        plotly::layout(
          xaxis = list(tickangle = -45),
          margin = list(b = 100, l = 150)
        )
    })
  })
}