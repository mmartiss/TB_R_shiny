ampliconUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Global controls shared by all analysis tabs
    h2("Amplicon Analysis"),
    fluidRow(
      column(2,
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
             tags$label("Color palette"),
             selectInput(ns("color_palette"), NULL,
                         choices = c("Tableau 10", "Pastel", "Bold", "Viridis-disc", "Earth"),
                         selected = "Tableau 10")
      ),
      column(3,
             tags$label("Sort Samples By"),
             selectInput(ns("sort_by"), NULL,
                         choices = c("Alphabetical" = "alpha",
                                     "Dendrogram (Cluster Analysis)" = "dendro",
                                     "Shannon Diversity" = "Shannon",
                                     "Observed OTUs" = "Observed",
                                     "Bray-Curtis PCoA" = "bray", 
                                     "Jaccard PCoA" = "jaccard",
                                     "Unweighted UniFrac PCoA" = "unifrac", 
                                     "Weighted UniFrac PCoA" = "wunifrac"),
                         selected = "alpha")
      ),
      column(2,
             tags$label("Sample Type Filter"),
             selectInput(ns("sample_suffix_filter"), NULL,
                         choices = c("All Samples" = "all", 
                                     "Only -S (S)" = "-S", 
                                     "Only -T (T)" = "-T"),
                         selected = "all")
      ),
      column(3,
             tags$br(),
             actionButton(ns("btn_generate"), "Update All Analysis", class = "btn-success w-100")
      )
    ),
    hr(),
    # Analysis tabs: Bar Chart -> Heatmap -> Alpha -> Beta -> Metadata
    tabsetPanel(
      # Taxonomic composition (stacked relative abundance)
      tabPanel("Taxonomic Bar Chart", 
               uiOutput(ns("color_editor_ui")),
               div(style = "overflow-x: auto; width: 100%;", 
                   plotlyOutput(ns("plot_bar"), width = "2000px", height = "800px")
               )),
      
      # Taxon-by-sample abundance heatmap
      tabPanel("Heatmap", 
               plotlyOutput(ns("plot_heatmap"), height = "800px")),
      
      # Within-sample diversity metrics
      tabPanel("Alpha Diversity", 
               br(),
               wellPanel(
                 fluidRow(
                   column(6,
                          checkboxGroupInput(ns("alpha_metrics_sel"), "Select Metrics:",
                                             choices = c("Observed (Unique)" = "Observed", 
                                                         "Shannon" = "Shannon", 
                                                         "Simpson" = "Simpson", 
                                                         "InvSimpson" = "InvSimpson"),
                                             selected = c("Observed", "Shannon"), inline = TRUE)
                   ),
                   column(6,
                          uiOutput(ns("alpha_group_by_ui")),
                          uiOutput(ns("alpha_group_filter_ui")) 
                   )
                 )
               ),
               plotlyOutput(ns("plot_alpha_multi"), height = "500px")),
      
              # Between-sample distance ordination (PCoA)
      tabPanel("Beta Diversity",
               br(),
               fluidRow(
                 column(3,
                        selectInput(ns("beta_metric"), "Distance Metric:",
                                    choices = c("Bray-Curtis" = "bray", "Jaccard" = "jaccard",
                                                "Unweighted UniFrac" = "unifrac", "Weighted UniFrac" = "wunifrac"),
                                    selected = "bray"),
                        hr(),
                        selectInput(ns("beta_color_type"), "Color Samples By:",
                                    choices = c("Uniform" = "none", "Metadata Category" = "meta", "Taxon Abundance" = "tax", "Dominant Taxon (Discrete Colors)" = "dominant")),
                        uiOutput(ns("beta_color_var_ui")),
                        hr(),
                        checkboxInput(ns("beta_show_biplot"), "Show Taxa Influences (Biplot)", value = FALSE),
                        conditionalPanel(
                          condition = sprintf("input['%s'] == true", ns("beta_show_biplot")),
                          numericInput(ns("beta_biplot_n"), "Number of Top Taxa:", value = 5, min = 1, max = 20)
                        )
                 ),
                 column(9, 
                        helpText("PCoA plot shows sample similarity. UniFrac requires a phylogenetic tree."),
                        plotlyOutput(ns("plot_beta_multi"), height = "600px")
                 )
               )
      ),
              # Taxon composition stratified by metadata categories
      tabPanel("Metadata Analysis",
               br(),
               fluidRow(
                 column(3,
                        wellPanel(
                          h4("Plot Settings"),
                          selectInput(ns("meta_x"), "X Axis (Category):", choices = NULL),
                          selectInput(ns("meta_facet_row"), "Facet Row (Optional):", choices = c("None" = ".")),
                          selectInput(ns("meta_facet_col"), "Facet Column (Optional):", choices = c("None" = ".")),
                          checkboxInput(ns("meta_show_points"), "Show individual samples", value = FALSE),
                          hr(),
                          uiOutput(ns("meta_val_filters_ui")),
                          hr(),
                          helpText("Choose which categories to include in the plot.")
                        )
                 ),
                 column(9,
                        plotlyOutput(ns("plot_metadata_bar"), height = "1000px")
                 )
               )
      )
    )
  )
}
ampliconServer <- function(id, data_res) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # =====================================================================
    # CODE NAVIGATION MAP
    # 1) Shared preprocessing + ordering
    # 2) Alpha Diversity
    # 3) Beta Diversity
    # 4) Taxonomic Bar Chart + Heatmap
    # 5) Metadata Analysis
    # =====================================================================
    
    # -----------------------------
    # 1) Shared preprocessing core
    # -----------------------------

    # Reusable color palettes for taxa
    PALETTES <- list(
      "Tableau 10"   = c("#4E79A7","#F28E2B","#E15759","#76B7B2","#59A14F","#EDC948","#B07AA1","#FF9DA7","#9C755F","#BAB0AC"),
      "Pastel"       = c("#AEC6CF","#FFD1DC","#B5EAD7","#FFDAC1","#C7CEEA","#E2F0CB","#F2D7D9","#D5E8D4","#DAE8FC","#F8CECC"),
      "Bold"         = c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#A65628","#F781BF","#999999","#66C2A5","#FC8D62"),
      "Viridis-disc" = c("#440154","#30678D","#35B779","#FDE724","#482677","#1F9E89","#73D055","#DCE319","#3CBB75","#94D840"),
      "Earth"        = c("#8C510A","#BF812D","#DFC27D","#80CDC1","#35978F","#01665E","#543005","#F6E8C3","#C7EAE5","#003C30")
    )
    
    # Runtime map of taxon -> color (used by multiple plots)
    rv_colors <- reactiveValues(map = list())
    
    # Safe metadata reader (returns NULL when metadata is unavailable)
    safe_meta <- reactive({
      tryCatch(data_res$meta(), error = function(e) NULL)
    })
    
    # Master preprocessing: abundance + taxonomy -> long + matrix formats
    processed_counts <- eventReactive(input$btn_generate, {
      req(data_res$abundance, data_res$taxonomy)
      ab <- data_res$abundance(); tax <- data_res$taxonomy(); level <- input$tax_level
      
      all_cols <- names(ab)
      sample_cols <- all_cols[sapply(ab, is.numeric) & all_cols != "tax_id"]
      
      ab$tax_id <- as.character(ab$tax_id); tax$tax_id <- as.character(tax$tax_id)
      merged <- dplyr::inner_join(ab, tax, by = "tax_id")
      merged[[level]][is.na(merged[[level]]) | merged[[level]] == "" | merged[[level]] == "unclassified"] <- "Unknown"
      
      long_counts <- merged %>%
        dplyr::select(dplyr::all_of(c(level, sample_cols))) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(sample_cols), names_to = "sample", values_to = "counts") %>%
        dplyr::mutate(sample = gsub("\\.fastq.*$|\\.fq.*$", "", sample))
      
      if (input$sample_suffix_filter != "all") {
        suffix <- input$sample_suffix_filter
        long_counts <- long_counts %>% 
          dplyr::filter(grepl(paste0(suffix, "$"), sample))
      }
      
      long_counts <- long_counts %>%
        dplyr::group_by(sample, taxon = .data[[level]]) %>%
        dplyr::summarise(counts = sum(counts, na.rm = TRUE), .groups = "drop")
      
      valid_samples <- long_counts %>% 
        dplyr::group_by(sample) %>% 
        dplyr::summarise(t = sum(counts)) %>% 
        dplyr::filter(t > 0) %>% 
        dplyr::pull(sample)
      
      long_counts <- long_counts %>% dplyr::filter(sample %in% valid_samples)
      
      matrix_counts <- long_counts %>% 
        tidyr::pivot_wider(names_from = taxon, values_from = counts, values_fill = 0) %>% 
        tibble::column_to_rownames("sample")
      
      list(long = long_counts, matrix = matrix_counts)
    })
    
    # Shared sample ordering logic used by Bar Chart and Heatmap
    sample_order <- reactive({
      req(processed_counts(), input$sort_by)
      mat <- processed_counts()$matrix
      all_samples <- rownames(mat)
      
      if (input$sort_by == "alpha") return(sort(all_samples))
      
      if (input$sort_by %in% c("Shannon", "Observed")) {
        req(alpha_data())
        ord <- alpha_data() %>% 
          dplyr::filter(Metric == input$sort_by) %>% 
          dplyr::arrange(Value) %>% 
          dplyr::pull(Sample)
        final_ord <- intersect(ord, all_samples)
        return(c(final_ord, setdiff(all_samples, final_ord)))
      }
      
      metric <- if(input$sort_by == "dendro") input$beta_metric else input$sort_by
      dist_mat <- NULL
      
      tryCatch({
        if (metric %in% c("bray", "jaccard")) {
          dist_mat <- vegan::vegdist(mat, method = metric, binary = (metric == "jaccard"))
        } else if (metric %in% c("unifrac", "wunifrac")) {
          req(data_res$tree(), data_res$abundance())
          ab <- data_res$abundance(); tree_raw <- data_res$tree()
          common_ids <- intersect(as.character(ab$tax_id), as.character(tree_raw$tip.label))
          if(length(common_ids) < 2) stop("Too few common taxa")
          
          sample_cols <- names(ab)[sapply(ab, is.numeric) & names(ab) != "tax_id"]
          mat_ids <- ab %>% 
            dplyr::filter(tax_id %in% common_ids) %>%
            tidyr::pivot_longer(cols = dplyr::all_of(sample_cols), names_to = "sample", values_to = "val") %>%
            dplyr::mutate(sample = gsub("\\.fastq.*$|\\.fq.*$", "", sample)) %>% 
            dplyr::group_by(sample, tax_id) %>% 
            dplyr::summarise(val = sum(as.numeric(val), na.rm = TRUE), .groups = "drop") %>%
            tidyr::pivot_wider(names_from = tax_id, values_from = val, values_fill = 0) %>%
            tibble::column_to_rownames("sample") %>% as.matrix()
          
          mat_ids <- mat_ids[intersect(rownames(mat_ids), all_samples), , drop=FALSE]
          curr_tree <- ape::keep.tip(tree_raw, colnames(mat_ids))
          curr_tree <- phytools::midpoint.root(curr_tree); curr_tree <- ape::multi2di(curr_tree)
          if (is.null(curr_tree$edge.length)) curr_tree$edge.length <- rep(0.001, nrow(curr_tree$edge))
          
          ps <- phyloseq::phyloseq(phyloseq::otu_table(mat_ids, taxa_are_rows = FALSE), phyloseq::phy_tree(curr_tree))
          dist_mat <- if(metric == "wunifrac") {
            phyloseq::UniFrac(phyloseq::transform_sample_counts(ps, function(x) x / sum(x)), weighted = TRUE)
          } else {
            phyloseq::UniFrac(ps, weighted = FALSE)
          }
        }
        
        if (!is.null(dist_mat)) {
          if (input$sort_by == "dendro") {
            hc <- hclust(dist_mat, method = "ward.D2")
            return(hc$labels[hc$order])
          } else {
            pcoa_res <- cmdscale(dist_mat, k = 1)
            ord <- rownames(pcoa_res)[order(pcoa_res[,1])]
            final_ord <- intersect(ord, all_samples)
            return(c(final_ord, setdiff(all_samples, final_ord)))
          }
        }
      }, error = function(e) {
        showNotification(paste("Sorting error:", e$message), type = "warning")
      })
      
      return(all_samples)
    })
    
    # Prepare Top-N taxa and relative abundance for stacked bar plotting
    prepared_bar_data <- reactive({
      req(processed_counts(), sample_order())
      ld <- processed_counts()$long
      top_taxa <- ld %>% dplyr::group_by(taxon) %>% dplyr::summarise(m = mean(counts)) %>% dplyr::filter(taxon != "Unknown") %>% dplyr::arrange(desc(m)) %>% dplyr::slice_head(n = input$top_n) %>% dplyr::pull(taxon)
      
      ld %>% 
        dplyr::mutate(taxon = ifelse(taxon %in% top_taxa, taxon, "Other")) %>% 
        dplyr::group_by(sample, taxon) %>% 
        dplyr::summarise(counts = sum(counts), .groups = "drop") %>% 
        dplyr::group_by(sample) %>% 
        dplyr::mutate(rel_abund = counts / sum(counts)) %>% 
        dplyr::ungroup() %>%
        dplyr::mutate(sample = factor(sample, levels = sample_order()))
    })
    
    # Build/update active color map for currently visible taxa
    # -----------------------------
    # 3) Beta Diversity
    # -----------------------------

    # Keep beta color mode choices aligned to available metadata
    observe({
      req(prepared_bar_data()); taxa <- unique(prepared_bar_data()$taxon); pal <- PALETTES[[input$color_palette]]
      cols <- rep_len(pal, length(taxa)); names(cols) <- taxa
      if("Other" %in% names(cols)) cols["Other"] <- "#D3D3D3"
      if("Unknown" %in% names(cols)) cols["Unknown"] <- "#A9A9A9"
      rv_colors$map <- as.list(cols)
    })
    
    # Per-taxon color editor UI
    output$color_editor_ui <- renderUI({
      req(rv_colors$map); taxa <- names(rv_colors$map)
      tags$div(style = "display:flex; flex-wrap:wrap; gap:10px; padding: 10px; background: #f9f9f9; border-bottom: 1px solid #ddd;",
               lapply(taxa, function(t) {
                 input_id <- paste0("col_", gsub("[^A-Za-z0-9]", "_", t))
                 tags$div(style = "display:flex; align-items:center; gap:5px;",
                          tags$input(type = "color", value = rv_colors$map[[t]], onchange = sprintf("Shiny.setInputValue('%s', this.value)", ns(input_id)), style="width:20px; height:20px; border:none;"),
                          tags$span(t, style="font-size:10px;"))
               }))
    })
    
    # -----------------------------
    # 2) Alpha Diversity
    # -----------------------------

    # UI: choose metadata column for grouping alpha metrics
    output$alpha_group_by_ui <- renderUI({
      m <- safe_meta()
      choices <- c("No Grouping" = "none")
      if (!is.null(m) && ncol(m) > 0) {
        choices <- c(choices, names(m))
      }
      selectInput(ns("alpha_group_by"), "Group By (Metadata):", choices = choices)
    })
    
    # UI: choose subgroup values within selected metadata column
    output$alpha_group_filter_ui <- renderUI({
      req(input$alpha_group_by, input$alpha_group_by != "none")
      m <- safe_meta()
      req(m)
      
      col_name <- input$alpha_group_by
      vals <- sort(unique(as.character(m[[col_name]])))
      
      selectizeInput(ns("alpha_group_subset"), 
                     label = paste("Select values from", col_name, ":"),
                     choices = vals, 
                     selected = vals,
                     multiple = TRUE,
                     options = list(plugins = list('remove_button')))
    })
    
    # Final alpha table after grouping/filtering selection
    alpha_data_final <- reactive({
      df <- alpha_data()
      req(df)
      
      if (is.null(input$alpha_group_by) || input$alpha_group_by == "none") {
        df$Group <- "All Samples"
      } else {
        if (input$alpha_group_by %in% names(df)) {
          df$Group <- as.character(df[[input$alpha_group_by]])
          
          if (!is.null(input$alpha_group_subset)) {
            df <- df %>% dplyr::filter(Group %in% input$alpha_group_subset)
          }
        } else {
          df$Group <- "All Samples"
        }
      }
      
      df <- df %>%
        group_by(Metric, Group) %>%
        mutate(n_count = n()) %>%
        ungroup() %>%
        mutate(Group_Label = paste0(Group, "\n(n=", n_count, ")"))
      
      df
    })
    
    # Generic distance helper reused in dendrogram sorting and heatmap/bar dendrogram mode
    calc_dist_matrix <- function(mat, metric, tree) {
      if (metric %in% c("bray", "jaccard")) {
        return(vegan::vegdist(mat, method = metric, binary = (metric == "jaccard")))
      } else {
        req(tree)
        otu <- phyloseq::otu_table(t(as.matrix(mat)), taxa_are_rows = TRUE)
        ps <- phyloseq::phyloseq(otu, tree)
        return(phyloseq::distance(ps, method = metric))
      }
    }
    
    # Raw alpha metrics computation table (Sample, Metric, Value)
    alpha_data <- reactive({
      req(processed_counts())
      
      raw_mat <- processed_counts()$matrix
      
      mat_numeric <- as.data.frame(raw_mat)
      mat_numeric <- mat_numeric[, sapply(mat_numeric, is.numeric), drop = FALSE]
      
      mat_numeric <- as.matrix(mat_numeric)
      
      # Metric formulas used in Alpha Diversity panel
      observed <- rowSums(mat_numeric > 0)
      shannon <- vegan::diversity(mat_numeric, index = "shannon")
      simpson <- vegan::diversity(mat_numeric, index = "simpson")
      invsimpson <- vegan::diversity(mat_numeric, index = "invsimpson")
      
      df <- data.frame(
        Sample = rownames(mat_numeric),
        Observed = as.numeric(observed),
        Shannon = as.numeric(shannon),
        Simpson = as.numeric(simpson),
        InvSimpson = as.numeric(invsimpson),
        stringsAsFactors = FALSE
      )
      
      df_long <- df %>%
        tidyr::pivot_longer(cols = -Sample, names_to = "Metric", values_to = "Value")
      
      metric_levels <- c("Observed", "Shannon", "Simpson", "InvSimpson")
      df_long$Metric <- factor(df_long$Metric, levels = metric_levels)
      
      m <- safe_meta()
      if (!is.null(m) && !is.null(input$alpha_group_by) && input$alpha_group_by != "none") {
        m_df <- as.data.frame(m)
        
        m_df$INTERNAL_JOIN_ID <- NA
        if (any(df_long$Sample %in% rownames(m_df))) {
          m_df$INTERNAL_JOIN_ID <- rownames(m_df)
        } else {
          for(col in names(m_df)) {
            if(any(df_long$Sample %in% as.character(m_df[[col]]))) {
              m_df$INTERNAL_JOIN_ID <- as.character(m_df[[col]])
              break
            }
          }
        }
        
        if (!all(is.na(m_df$INTERNAL_JOIN_ID))) {
          df_long <- df_long %>% 
            dplyr::left_join(m_df, by = c("Sample" = "INTERNAL_JOIN_ID"))
        }
      }
      
      df_long
    })
    
    # Output: Alpha Diversity multi-panel boxplot
    output$plot_alpha_multi <- renderPlotly({
      df <- alpha_data_final() %>% 
        dplyr::filter(Metric %in% input$alpha_metrics_sel & !is.na(Value))
      
      if(nrow(df) == 0) return(NULL)
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Group_Label, y = Value, fill = Group)) +
        ggplot2::geom_boxplot(alpha = 0.6, outlier.shape = NA) +
        ggplot2::geom_jitter(ggplot2::aes(text = paste("Sample:", Sample, "<br>Value:", round(Value, 3))), 
                             width = 0.15, size = 2, alpha = 0.7) +
        ggplot2::facet_wrap(~Metric, scales = "free", nrow = 1) + 
        ggplot2::theme_minimal() +
        ggplot2::labs(x = NULL, y = "Index Value") +
        ggplot2::theme(
          legend.position = "none",
          strip.background = ggplot2::element_rect(fill = "#f0f0f0", color = NA),
          strip.text = ggplot2::element_text(face = "bold", size = 12),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
      
      if (length(unique(df$Group)) <= 12) {
        p <- p + ggplot2::scale_fill_brewer(palette = "Set3")
      }
      
      plotly::ggplotly(p, tooltip = "text")
    })
    
    observe({
      m <- data_res$meta()
      choices <- c("Uniform" = "none", "Taxon Abundance" = "tax", "Dominant Taxon (Discrete Colors)" = "dominant")
      
      if (!is.null(m) && ncol(m) > 0) {
        choices <- c(choices, "Metadata Category" = "meta")
      }
      
      updateSelectInput(session, "beta_color_type", choices = choices, 
                        selected = isolate(input$beta_color_type))
    })
    
    
    # Attach metadata columns to beta coordinates via best-matching sample IDs
    get_merged_beta_data <- function(pcoa_df, meta_df) {
      if (is.null(meta_df) || inherits(meta_df, "try-error") || nrow(as.data.frame(meta_df)) == 0) {
        return(pcoa_df)
      }
      
      m <- as.data.frame(meta_df)
      found_id <- NULL
      if (any(pcoa_df$Sample %in% rownames(m))) {
        m$INTERNAL_ID <- rownames(m)
        found_id <- "INTERNAL_ID"
      } else {
        for (col in names(m)) {
          if (any(pcoa_df$Sample %in% as.character(m[[col]]))) {
            m$INTERNAL_ID <- as.character(m[[col]])
            found_id <- "INTERNAL_ID"
            break
          }
        }
      }
      
      if (is.null(found_id)) return(pcoa_df)
      
      dplyr::left_join(pcoa_df, m, by = c("Sample" = "INTERNAL_ID"))
    }
    
    # UI: dynamic color-variable selector for Beta plot
    output$beta_color_var_ui <- renderUI({
      req(input$beta_color_type)
      
      if (input$beta_color_type == "meta") {
        req(data_res$meta())
        choices <- names(data_res$meta())
        selectInput(ns("beta_color_meta"), "Select Metadata Column:", choices = choices)
        
      } else if (input$beta_color_type == "tax") {
        req(processed_counts())
        taxa_choices <- colnames(processed_counts()$matrix)
        selectizeInput(ns("beta_color_taxon"), "Select Taxon (or Taxa):", 
                       choices = taxa_choices, 
                       multiple = TRUE,
                       options = list(
                         placeholder = 'Search and select one or more taxa...',
                         maxOptions = 1000,
                         plugins = list('remove_button')
                       ))
      } else {
        NULL
      }
    })
    
    
    # PCoA coordinates and explained variance for selected beta distance
    beta_res <- reactive({
      req(processed_counts(), input$beta_metric)
      metric <- input$beta_metric
      
      if (!metric %in% c("unifrac", "wunifrac")) {
        mat <- processed_counts()$matrix
        dist_mat <- vegan::vegdist(mat, method = metric, binary = (metric == "jaccard"))
      } 
      else {
        req(data_res$tree(), data_res$abundance())
        ab <- data_res$abundance()
        tree_raw <- data_res$tree()
        
        ab$tax_id <- as.character(ab$tax_id)
        tree_raw$tip.label <- as.character(tree_raw$tip.label)
        
        common_ids <- intersect(ab$tax_id, tree_raw$tip.label)
        if (length(common_ids) < 2) {
          showNotification("Per mažai bendrų taksonų tarp medžio ir duomenų!", type = "error")
          return(NULL)
        }
        
        sample_cols <- names(ab)[sapply(ab, is.numeric) & names(ab) != "tax_id"]
        
        mat_ids <- ab %>% 
          dplyr::filter(tax_id %in% common_ids) %>%
          tidyr::pivot_longer(cols = dplyr::all_of(sample_cols), names_to = "sample", values_to = "val") %>%
          dplyr::mutate(sample = gsub("\\.fastq.*$|\\.fq.*$", "", sample)) %>% 
          dplyr::group_by(sample, tax_id) %>% 
          dplyr::summarise(val = sum(as.numeric(val), na.rm = TRUE), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = tax_id, values_from = val, values_fill = 0) %>%
          tibble::column_to_rownames("sample") %>% 
          as.matrix()
        
        mat_ids <- mat_ids[rowSums(mat_ids) > 0, , drop = FALSE]
        if (nrow(mat_ids) < 2) {
          showNotification("Per mažai mėginių su duomenimis po filtravimo!", type = "warning")
          return(NULL)
        }
        
        curr_tree <- ape::keep.tip(tree_raw, colnames(mat_ids))
        
        if (!ape::is.rooted(curr_tree)) {
          curr_tree <- phytools::midpoint.root(curr_tree)
        }
        
        curr_tree <- ape::multi2di(curr_tree)
        if (is.null(curr_tree$edge.length)) {
          curr_tree$edge.length <- rep(0.001, nrow(curr_tree$edge))
        } else {
          curr_tree$edge.length[curr_tree$edge.length <= 0] <- 0.00001
        }
        
        ps <- phyloseq::phyloseq(
          phyloseq::otu_table(mat_ids, taxa_are_rows = FALSE), 
          phyloseq::phy_tree(curr_tree)
        )
        
        dist_mat <- tryCatch({
          if (metric == "wunifrac") {
            ps_rel <- phyloseq::transform_sample_counts(ps, function(x) x / sum(x))
            phyloseq::UniFrac(ps_rel, weighted = TRUE, normalized = TRUE)
          } else {
            phyloseq::UniFrac(ps, weighted = FALSE)
          }
        }, error = function(e) {
          showNotification(paste("UniFrac klaida:", e$message), type = "error")
          return(NULL)
        })
      }
      
      req(dist_mat)
      
      pcoa_res <- cmdscale(dist_mat, k = 2, eig = TRUE, add = TRUE)
      points <- as.data.frame(pcoa_res$points)
      df <- data.frame(Sample = rownames(points), PC1 = points[, 1], PC2 = points[, 2])
      
      ev <- pcoa_res$eig
      ev[ev < 0] <- 0
      var_exp <- round(ev / sum(ev) * 100, 1)
      
      list(df = df, var = var_exp)
    })
    
    # Output: Beta Diversity PCoA plot (optional biplot vectors)
    output$plot_beta_multi <- renderPlotly({
      req(beta_res())
      res <- beta_res()
      df <- res$df 
      
      m_data <- safe_meta()
      df <- get_merged_beta_data(df, m_data)
      
      color_var <- NULL
      color_label <- "Mėginiai"
      hover_text <- paste0("<b>Sample:</b> ", df$Sample)
      is_numeric_color <- FALSE
      colors_to_use <- "Set1" 
      
      ctype <- input$beta_color_type
      
      if (ctype == "meta" && (is.null(m_data) || is.null(input$beta_color_meta))) {
        ctype <- "none"
      }
      
      if (ctype == "none") {
        color_var <- rep("Visi mėginiai", nrow(df))
        colors_to_use <- "#4E79A7"
        color_label <- "Grupė"
      } 
      else if (ctype == "meta") {
        req(input$beta_color_meta)
        if (input$beta_color_meta %in% names(df)) {
          color_var <- df[[input$beta_color_meta]]
          color_label <- input$beta_color_meta
        } else {
          color_var <- rep("Visi mėginiai", nrow(df))
        }
      } 
      else if (ctype == "tax") {
        req(input$beta_color_taxon)
        mat <- processed_counts()$matrix
        mat_rel <- sweep(mat, 1, rowSums(mat), "/") * 100
        
        sel <- input$beta_color_taxon
        if (length(sel) > 0) {
          valid_sel <- intersect(sel, colnames(mat_rel))
          combined_abund <- if(length(valid_sel) > 1) rowSums(mat_rel[, valid_sel]) else mat_rel[, valid_sel]
          
          tax_abund <- data.frame(Sample = rownames(mat_rel), Abund = combined_abund)
          df <- df %>% dplyr::left_join(tax_abund, by = "Sample")
          color_var <- df$Abund
          color_label <- "Abund. (%)"
          is_numeric_color <- TRUE
          hover_text <- paste0(hover_text, "<br>Kiekis: ", round(df$Abund, 2), "%")
        }
      }
      else if (ctype == "dominant") {
        req(processed_counts(), rv_colors$map)
        mat <- processed_counts()$matrix
        dom_taxa <- colnames(mat)[apply(mat, 1, which.max)]
        top_taxa_names <- names(rv_colors$map)
        dom_taxa_final <- ifelse(dom_taxa %in% top_taxa_names, dom_taxa, "Other")
        
        df$Dominant <- dom_taxa_final[match(df$Sample, rownames(mat))]
        color_var <- df$Dominant
        color_label <- "Dominuojantis"
        colors_to_use <- unlist(rv_colors$map)
      }
      
      p <- plotly::plot_ly()
      
      if (is_numeric_color) {
        p <- p %>% plotly::add_markers(
          data = df, x = ~PC1, y = ~PC2, text = hover_text,
          marker = list(size = 12, opacity = 0.8, color = color_var, 
                        colorscale = "Viridis", showscale = TRUE,
                        colorbar = list(title = color_label))
        )
      } else {
        p <- p %>% plotly::add_markers(
          data = df, x = ~PC1, y = ~PC2, color = color_var, text = hover_text,
          colors = colors_to_use, 
          marker = list(size = 12, opacity = 0.8, line = list(color = "#fff", width = 1))
        )
      }
      
      if (input$beta_show_biplot) {
        req(processed_counts())
        mat_rel <- sweep(as.matrix(processed_counts()$matrix), 1, rowSums(processed_counts()$matrix), "/")
        common <- intersect(rownames(mat_rel), df$Sample)
        mat_sub <- mat_rel[common, ]; df_sub <- df[match(common, df$Sample), ]
        
        taxa_pc1 <- colSums(mat_sub * df_sub$PC1) / colSums(mat_sub)
        taxa_pc2 <- colSums(mat_sub * df_sub$PC2) / colSums(mat_sub)
        
        loadings <- data.frame(Taxon = colnames(mat_sub), PC1 = taxa_pc1, PC2 = taxa_pc2)
        loadings$dist <- sqrt(loadings$PC1^2 + loadings$PC2^2)
        top_taxa <- loadings %>% dplyr::arrange(desc(dist)) %>% head(input$beta_biplot_n)
        
        for(i in 1:nrow(top_taxa)) {
          p <- p %>% plotly::add_segments(
            x = 0, y = 0, xend = top_taxa$PC1[i], yend = top_taxa$PC2[i],
            line = list(color = "black", width = 1), showlegend = FALSE, hoverinfo = "none"
          ) %>% plotly::add_annotations(
            x = top_taxa$PC1[i], y = top_taxa$PC2[i], text = top_taxa$Taxon[i],
            showarrow = FALSE, font = list(color = "black", size = 10)
          )
        }
      }
      
      p %>% plotly::layout(
        xaxis = list(title = paste0("PC1 (", res$var[1], "%)")),
        yaxis = list(title = paste0("PC2 (", res$var[2], "%)")),
        legend = list(title = list(text = color_label))
      )
    })
    
    # -----------------------------
    # 4) Taxonomic Bar Chart + Heatmap
    # -----------------------------

    # Base stacked barplot helper
    create_base_barplot <- function(df, cols, sample_order = NULL) {
      p <- plotly::plot_ly(
        df, 
        x = ~sample, 
        y = ~rel_abund, 
        color = ~taxon, 
        colors = cols, 
        type = "bar", 
        showlegend = TRUE, 
        hoverinfo = "text", 
        text = ~paste0(taxon, ": ", round(rel_abund * 100, 2), "%")
      ) %>% 
        plotly::layout(
          barmode = "stack",
          yaxis = list(title = "Relative Abundance", tickformat = ".0%"),
          xaxis = list(
            title = "",
            tickangle = 90,
            tickmode = "array",
            tickvals = sample_order,
            ticktext = sample_order,
            categoryorder = "array",
            categoryarray = sample_order
          ),  
          margin = list(b = 100)
        )           
      return(p)
    }
    
    # Dendrogram helper aligned to sample labels
    create_dendro_plot <- function(hc, ordered_samples) {
      dendro_data <- ggdendro::dendro_data(hc)
      seg <- dendro_data$segments
      
      seg$x_label <- ordered_samples[round(seg$x)]
      seg$xend_label <- ordered_samples[round(seg$xend)]
      
      plotly::plot_ly() %>%
        plotly::add_segments(
          data = seg,
          x = ~x_label, y = ~y, xend = ~xend_label, yend = ~yend,
          line = list(color = "#444", width = 1.5),
          showlegend = FALSE,
          hoverinfo = "none"
        ) %>%
        plotly::layout(
          xaxis = list(
            type = "category", 
            categoryorder = "array",
            categoryarray = ordered_samples,
            showticklabels = FALSE,
            showgrid = FALSE,
            title = ""
          ),
          yaxis = list(
            autorange = "reversed",
            zeroline = FALSE, 
            showgrid = FALSE,
            showticklabels = FALSE,
            title = ""
          )
        )
    }
    
    # Output: Taxonomic Bar Chart (normal or dendrogram-aligned)
    output$plot_bar <- renderPlotly({
      req(prepared_bar_data(), rv_colors$map, sample_order())
      
      df <- prepared_bar_data()
      cols <- unlist(rv_colors$map)
      
      if (input$sort_by != "dendro") {
        ord <- sample_order()        
        df$sample <- factor(df$sample, levels = ord) 
        return(create_base_barplot(df, cols, ord))
      }
      
      tryCatch({
        mat <- processed_counts()$matrix
        dist_mat <- calc_dist_matrix(mat, input$beta_metric, data_res$tree())
        hc <- hclust(dist_mat, method = "ward.D2")
        ordered_samples <- hc$labels[hc$order]
        
        df$sample <- factor(df$sample, levels = ordered_samples)
        
        p_bar <- create_base_barplot(df, cols, ordered_samples)
        p_tree <- create_dendro_plot(hc, ordered_samples)
        
        plotly::subplot(p_bar, p_tree, nrows = 2, heights = c(0.8, 0.2),
                        shareX = FALSE, titleY = TRUE, margin = c(0.01, 0.01, 0.01, 0.08)) %>%
          plotly::layout(
            xaxis = list(
              tickangle = 90,
              tickmode = "array",
              tickvals = ordered_samples,
              ticktext = ordered_samples,
              showticklabels = TRUE
            ),
            xaxis2 = list(
              showticklabels = FALSE,
              categoryorder = "array",
              categoryarray = ordered_samples,
              matches = "x"
            ),
            margin = list(b = 150)
          )
        
      }, error = function(e) {
        message("Dendrogram error: ", e)
        create_base_barplot(df, cols) %>% 
          plotly::layout(title = "Dendrogram error - showing unsorted plot")
      })
    })
    
    # Output: Heatmap (normal or dendrogram-aligned)
    output$plot_heatmap <- renderPlotly({
      req(processed_counts(), sample_order())
      mat <- as.matrix(processed_counts()$matrix)
      
      mat_rel <- sweep(mat, 1, rowSums(mat), "/")
      top_n_taxa <- names(sort(colMeans(mat_rel), decreasing = TRUE))[1:min(input$top_n, ncol(mat_rel))]
      
      if (input$sort_by != "dendro") {
        avail_samples <- intersect(sample_order(), rownames(mat_rel))
        mat_plot <- t(mat_rel[avail_samples, top_n_taxa, drop = FALSE])
        
        plotly::plot_ly(
          x = colnames(mat_plot),
          y = rownames(mat_plot),
          z = mat_plot,
          type = "heatmap",
          colorscale = "Viridis"
        ) %>%
          plotly::layout(
            xaxis = list(
              tickangle = 90,
              tickmode = "array",
              tickvals = avail_samples,
              ticktext = avail_samples,
              categoryorder = "array",
              categoryarray = avail_samples
            ),
            margin = list(b = 150, l = 150)
          )
        
      } else {
        tryCatch({
          dist_mat <- calc_dist_matrix(mat, input$beta_metric, data_res$tree())
          hc <- hclust(dist_mat, method = "ward.D2")
          ordered_samples <- hc$labels[hc$order]
          
          avail_samples <- intersect(ordered_samples, rownames(mat_rel))
          mat_plot <- t(mat_rel[avail_samples, top_n_taxa, drop = FALSE])
          
          p_heat <- plotly::plot_ly(
            x = colnames(mat_plot),
            y = rownames(mat_plot),
            z = mat_plot,
            type = "heatmap",
            colorscale = "Viridis"
          ) %>%
            plotly::layout(
              xaxis = list(
                tickangle = 90,
                tickmode = "array",
                tickvals = avail_samples,
                ticktext = avail_samples,
                categoryorder = "array",
                categoryarray = avail_samples,
                title = ""
              ),
              yaxis = list(title = ""),
              margin = list(b = 150, l = 150)
            )
          
          p_tree <- create_dendro_plot(hc, avail_samples)
          
          plotly::subplot(
            p_heat, p_tree,
            nrows = 2,
            heights = c(0.8, 0.2),
            shareX = FALSE,
            titleY = TRUE,
            margin = c(0.01, 0.01, 0.01, 0.08)
          ) %>%
            plotly::layout(
              xaxis2 = list(
                showticklabels = FALSE,
                categoryorder = "array",
                categoryarray = avail_samples,
                matches = "x"
              ),
              xaxis = list(
                tickangle = 90,
                tickmode = "array",
                tickvals = avail_samples,
                ticktext = avail_samples,
                categoryorder = "array",
                categoryarray = avail_samples
              ),
              margin = list(b = 150, l = 150)
            )
          
        }, error = function(e) {
          message("Heatmap dendrogram error: ", e)
          avail_samples <- intersect(sample_order(), rownames(mat_rel))
          mat_plot <- t(mat_rel[avail_samples, top_n_taxa, drop = FALSE])
          
          plotly::plot_ly(
            x = colnames(mat_plot),
            y = rownames(mat_plot),
            z = mat_plot,
            type = "heatmap",
            colorscale = "Viridis"
          ) %>%
            plotly::layout(
              title = "Dendrogram error – showing unsorted heatmap",
              xaxis = list(tickangle = 90),
              margin = list(b = 150, l = 150)
            )
        })
      }
    })
    
    # -----------------------------
    # 5) Metadata Analysis
    # -----------------------------

    # Populate metadata axes/facet selectors from current metadata table
    observe({
      req(data_res$meta)
      m <- data_res$meta()
      req(m)
      
      meta_cols <- names(m)
      updateSelectInput(session, "meta_x", 
                        choices = c("Sample Name" = "INTERNAL_SAMPLE_ID", meta_cols))
      updateSelectInput(session, "meta_facet_row", choices = c("None" = ".", meta_cols))
      updateSelectInput(session, "meta_facet_col", choices = c("None" = ".", meta_cols))
    })
    
    # Metadata columns currently active in x-axis or facets
    active_meta_cols <- reactive({
      cols <- c(input$meta_x, input$meta_facet_row, input$meta_facet_col)
      unique(cols[cols != "." & cols != "INTERNAL_SAMPLE_ID"])
    })
    
    # UI: value-level filters for active metadata columns
    output$meta_val_filters_ui <- renderUI({
      req(active_meta_cols(), data_res$meta())
      m <- as.data.frame(data_res$meta())
      
      tagList(
        lapply(active_meta_cols(), function(col_name) {
          vals <- sort(unique(as.character(m[[col_name]])))
          
          clean_id <- paste0("filter_val_", gsub("[^A-Za-z0-9]", "_", col_name))
          
          selectizeInput(ns(clean_id), 
                         label = paste("Keep in:", col_name),
                         choices = vals, 
                         selected = vals,
                         multiple = TRUE,
                         options = list(plugins = list('remove_button')))
        })
      )
    })
    
    # Data preparation for metadata-stratified stacked composition chart
    metadata_plot_data <- reactive({
      req(processed_counts(), data_res$meta, input$meta_x)
      m <- as.data.frame(data_res$meta())
      ld <- processed_counts()$long
      
      meta_df <- m
      meta_df$INTERNAL_SAMPLE_ID <- rownames(m)
      for(col in names(m)) {
        if(any(ld$sample %in% as.character(m[[col]]))) {
          meta_df$INTERNAL_SAMPLE_ID <- as.character(m[[col]])
          break
        }
      }
      
      plot_df <- ld %>%
        dplyr::inner_join(meta_df, by = c("sample" = "INTERNAL_SAMPLE_ID"))
      
      for(col_name in active_meta_cols()) {
        clean_id <- paste0("filter_val_", gsub("[^A-Za-z0-9]", "_", col_name))
        selected_vals <- input[[clean_id]]
        
        if (!is.null(selected_vals)) {
          plot_df <- plot_df %>% dplyr::filter(.data[[col_name]] %in% selected_vals)
        }
      }
      
      if (input$meta_x == "INTERNAL_SAMPLE_ID" || input$meta_show_points) {
        group_vars <- c("sample", "taxon")
        if(input$meta_facet_row != ".") group_vars <- c(group_vars, input$meta_facet_row)
        if(input$meta_facet_col != ".") group_vars <- c(group_vars, input$meta_facet_col)
        
        final_df <- plot_df %>%
          dplyr::group_by(sample) %>%
          dplyr::mutate(rel_abund = counts / sum(counts, na.rm = TRUE)) %>%
          dplyr::group_by_at(group_vars) %>%
          dplyr::summarise(plot_value = sum(rel_abund), .groups = "drop") %>%
          dplyr::rename(display_x = sample)
      } else {
        group_vars <- c(input$meta_x, "taxon")
        if(input$meta_facet_row != ".") group_vars <- c(group_vars, input$meta_facet_row)
        if(input$meta_facet_col != ".") group_vars <- c(group_vars, input$meta_facet_col)
        
        final_df <- plot_df %>%
          dplyr::group_by(sample) %>%
          dplyr::mutate(rel_abund = counts / sum(counts, na.rm = TRUE)) %>%
          dplyr::group_by_at(group_vars) %>%
          dplyr::summarise(plot_value = mean(rel_abund, na.rm = TRUE), .groups = "drop") %>%
          dplyr::rename(display_x = !!sym(input$meta_x))
      }
      final_df
    })
    
    # Output: Metadata Analysis stacked bar chart
    output$plot_metadata_bar <- renderPlotly({
      req(metadata_plot_data(), rv_colors$map)
      df <- metadata_plot_data()
      
      df <- df %>% dplyr::filter(plot_value > 0)
      
      df$taxon <- factor(df$taxon, levels = intersect(names(rv_colors$map), unique(df$taxon)))
      
      all_cols <- unlist(rv_colors$map)
      present_taxa <- as.character(unique(df$taxon))
      cols <- all_cols[names(all_cols) %in% present_taxa]
      
      y_label <- if(input$meta_show_points) "Relative frequency" else "Average relative frequency"
      
      p <- ggplot2::ggplot(df, ggplot2::aes(
        x = display_x, 
        y = plot_value, 
        fill = taxon,
        text = paste0("Sample/Group: ", display_x, "<br>Type: ", taxon, "<br>Part: ", round(plot_value*100, 2), "%")
      )) +
        ggplot2::geom_bar(stat = "identity", position = "stack", width = 0.8) +
        ggplot2::scale_fill_manual(values = cols) +
        ggplot2::labs(y = y_label, x = NULL, fill = "Rūšis") +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = 8))
      
      if(input$meta_facet_row != "." || input$meta_facet_col != ".") {
        safe_row <- if(input$meta_facet_row == ".") "." else paste0("`", input$meta_facet_row, "`")
        safe_col <- if(input$meta_facet_col == ".") "." else paste0("`", input$meta_facet_col, "`")
        p <- p + ggplot2::facet_grid(as.formula(paste(safe_row, "~", safe_col)), scales = "free_x", space = "free_x")
      }
      
      plotly::ggplotly(p, tooltip = "text") %>% 
        plotly::layout(
          legend = list(orientation = "h", y = -0.3),
          margin = list(b = 100)
        )
    })
  })
}