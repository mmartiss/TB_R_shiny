# Reikalingos bibliotekos: shiny, dplyr, tidyr, plotly, vegan, phyloseq, ape, phytools

ampliconUI <- function(id) {
  ns <- NS(id)
  tagList(
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
      column(3,
             tags$br(),
             actionButton(ns("btn_generate"), "Update All Analysis", class = "btn-success w-100")
      )
    ),
    hr(),
    tabsetPanel(
      tabPanel("Taxonomic Bar Chart", 
               uiOutput(ns("color_editor_ui")),
               plotlyOutput(ns("plot_bar"), height = "800px")),
      
      tabPanel("Heatmap", 
               plotlyOutput(ns("plot_heatmap"), height = "800px")),
      
      tabPanel("Alpha Diversity", 
               br(),
               wellPanel(
                 checkboxGroupInput(ns("alpha_metrics_sel"), "Select Metrics:",
                                    choices = c("Observed (Unique)" = "Observed", 
                                                "Shannon" = "Shannon", 
                                                "Simpson" = "Simpson", 
                                                "InvSimpson" = "InvSimpson"),
                                    selected = c("Observed", "Shannon"), inline = TRUE)
               ),
               plotlyOutput(ns("plot_alpha_multi"), height = "500px")),
      
      tabPanel("Beta Diversity",
               br(),
               fluidRow(
                 column(4,
                        selectInput(ns("beta_metric"), "Distance Metric (Plot only):",
                                    choices = c("Bray-Curtis" = "bray", 
                                                "Jaccard (Binary)" = "jaccard",
                                                "Unweighted UniFrac" = "unifrac", 
                                                "Weighted UniFrac" = "wunifrac"),
                                    selected = "bray")
                 ),
                 column(8, helpText("PCoA plot shows sample similarity. UniFrac requires a phylogenetic tree."))
               ),
               hr(),
               plotlyOutput(ns("plot_beta_multi"), height = "600px")
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
    
    # --- DUOMENŲ APDOROJIMAS ---
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
        dplyr::mutate(sample = gsub("\\.fastq.*$|\\.fq.*$", "", sample)) %>%
        dplyr::group_by(sample, taxon = .data[[level]]) %>%
        dplyr::summarise(counts = sum(counts, na.rm = TRUE), .groups = "drop")
      
      valid_samples <- long_counts %>% dplyr::group_by(sample) %>% dplyr::summarise(t = sum(counts)) %>% dplyr::filter(t > 0) %>% dplyr::pull(sample)
      long_counts <- long_counts %>% dplyr::filter(sample %in% valid_samples)
      
      matrix_counts <- long_counts %>% tidyr::pivot_wider(names_from = taxon, values_from = counts, values_fill = 0) %>% tibble::column_to_rownames("sample")
      list(long = long_counts, matrix = matrix_counts)
    })
    
    # --- RIKIAVIMO LOGIKA ---
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
    
    observe({
      req(prepared_bar_data()); taxa <- unique(prepared_bar_data()$taxon); pal <- PALETTES[[input$color_palette]]
      cols <- rep_len(pal, length(taxa)); names(cols) <- taxa
      if("Other" %in% names(cols)) cols["Other"] <- "#D3D3D3"
      if("Unknown" %in% names(cols)) cols["Unknown"] <- "#A9A9A9"
      rv_colors$map <- as.list(cols)
    })
    
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
    
    # --- ALPHA DIVERSITY ---
    alpha_data <- reactive({
      req(processed_counts()); mat <- processed_counts()$matrix
      data.frame(Sample = rownames(mat), Observed = rowSums(mat > 0), Shannon = vegan::diversity(mat, index = "shannon"),
                 Simpson = vegan::diversity(mat, index = "simpson"), InvSimpson = vegan::diversity(mat, index = "invsimpson")) %>%
        tidyr::pivot_longer(cols = -Sample, names_to = "Metric", values_to = "Value")
    })
    
    output$plot_alpha_multi <- renderPlotly({
      req(alpha_data(), input$alpha_metrics_sel)
      df <- alpha_data() %>% dplyr::filter(Metric %in% input$alpha_metrics_sel & !is.na(Value))
      if(nrow(df) == 0) return(NULL)
      p <- suppressWarnings({
        ggplot2::ggplot(df, ggplot2::aes(x = Metric, y = Value, fill = Metric)) +
          ggplot2::geom_boxplot(alpha = 0.6, outlier.shape = NA) +
          ggplot2::geom_jitter(ggplot2::aes(text = paste("Sample:", Sample, "<br>Value:", round(Value, 3))), width = 0.15, size = 2, alpha = 0.7) +
          ggplot2::facet_wrap(~Metric, scales = "free", nrow = 1) + ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none", axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                         strip.background = ggplot2::element_rect(fill = "#f0f0f0", color = NA), strip.text = ggplot2::element_text(face = "bold")) +
          ggplot2::scale_fill_brewer(palette = "Set2")
      })
      plotly::ggplotly(p, tooltip = "text")
    })
    
    # --- BETA DIVERSITY ---
    beta_res <- reactive({
      req(processed_counts(), input$beta_metric)
      metric <- input$beta_metric
      
      # 1. Jei tai paprasti metodai (Bray/Jaccard)
      if (!metric %in% c("unifrac", "wunifrac")) {
        mat <- processed_counts()$matrix
        dist_mat <- vegan::vegdist(mat, method = metric, binary = (metric == "jaccard"))
      } 
      # 2. Jei tai UniFrac (reikia medžio)
      else {
        req(data_res$tree(), data_res$abundance())
        ab <- data_res$abundance()
        tree_raw <- data_res$tree()
        
        # Užtikrinam tipų sutapimą
        ab$tax_id <- as.character(ab$tax_id)
        tree_raw$tip.label <- as.character(tree_raw$tip.label)
        
        # Surandam bendrus ID
        common_ids <- intersect(ab$tax_id, tree_raw$tip.label)
        if (length(common_ids) < 2) {
          showNotification("Per mažai bendrų taksonų tarp medžio ir duomenų!", type = "error")
          return(NULL)
        }
        
        # Paruošiam matrica UniFrac skaičiavimui
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
        
        # SVARBU: Pašalinam mėginius, kurie turi 0 reads po taksonų filtravimo
        # Weighted UniFrac lūžta, jei mėginio suma yra 0
        mat_ids <- mat_ids[rowSums(mat_ids) > 0, , drop = FALSE]
        if (nrow(mat_ids) < 2) {
          showNotification("Per mažai mėginių su duomenimis po filtravimo!", type = "warning")
          return(NULL)
        }
        
        # Medžio paruošimas
        curr_tree <- ape::keep.tip(tree_raw, colnames(mat_ids))
        
        # UniFrac BŪTINAI reikia šakninio medžio (Rooted tree)
        if (!ape::is.rooted(curr_tree)) {
          curr_tree <- phytools::midpoint.root(curr_tree)
        }
        
        # Užtikrinam, kad medis neturi "multi-phy" (polytomies) ir turi briaunų ilgius
        curr_tree <- ape::multi2di(curr_tree)
        if (is.null(curr_tree$edge.length)) {
          curr_tree$edge.length <- rep(0.001, nrow(curr_tree$edge))
        } else {
          curr_tree$edge.length[curr_tree$edge.length <= 0] <- 0.00001
        }
        
        # Sukuriam phyloseq objektą
        ps <- phyloseq::phyloseq(
          phyloseq::otu_table(mat_ids, taxa_are_rows = FALSE), 
          phyloseq::phy_tree(curr_tree)
        )
        
        # Skaičiuojam atstumus
        dist_mat <- tryCatch({
          if (metric == "wunifrac") {
            # Weighted UniFrac geriausia skaičiuoti ant proporcijų
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
      
      # PCoA analizė
      pcoa_res <- cmdscale(dist_mat, k = 2, eig = TRUE, add = TRUE)
      points <- as.data.frame(pcoa_res$points)
      df <- data.frame(Sample = rownames(points), PC1 = points[, 1], PC2 = points[, 2])
      
      ev <- pcoa_res$eig
      ev[ev < 0] <- 0
      var_exp <- round(ev / sum(ev) * 100, 1)
      
      list(df = df, var = var_exp)
    })
    
    output$plot_beta_multi <- renderPlotly({
      req(beta_res()); res <- beta_res()
      plotly::plot_ly(res$df, x = ~PC1, y = ~PC2, text = ~paste0("<b>Sample:</b> ", Sample),
                      type = "scatter", mode = "markers", marker = list(size = 12, color = "#E15759")) %>%
        plotly::layout(xaxis = list(title = paste0("PC1 (", res$var[1], "%)")), yaxis = list(title = paste0("PC2 (", res$var[2], "%)")))
    })
    
    # --- BAR & HEATMAP ---
    # --- PAGALBINĖS FUNKCIJOS (galima dėti už serverio ribų arba serverio pradžioje) ---
    
    # 1. Funkcija baziniam Barplot sukurti
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
        # GERAI - pataisyta
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
    
    # 2. Funkcija dendrogramos braižymui
    create_dendro_plot <- function(hc, ordered_samples) {
      dendro_data <- ggdendro::dendro_data(hc)
      seg <- dendro_data$segments
      
      # Vietoj skaičių (1, 2, 3) priskiriame tikrus pavadinimus pagal indeksą
      seg$x_label <- ordered_samples[round(seg$x)]
      seg$xend_label <- ordered_samples[round(seg$xend)]
      
      plotly::plot_ly() %>%
        plotly::add_segments(
          data = seg,
          # Naudojame pavadinimus, o ne skaičius!
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
    
    # --- SHINY OUTPUT ---
    
    output$plot_bar <- renderPlotly({
      req(prepared_bar_data(), rv_colors$map, sample_order())
      
      df <- prepared_bar_data()
      cols <- unlist(rv_colors$map)

      if (input$sort_by != "dendro") {
        ord <- sample_order()        
        df$sample <- factor(df$sample, levels = ord) 
        return(create_base_barplot(df, cols, ord))
      }
      
      # Dendrogramos atvejis
      tryCatch({
        # A. Skaičiavimai
        mat <- processed_counts()$matrix
        dist_mat <- vegan::vegdist(mat, method = input$beta_metric)
        hc <- hclust(dist_mat, method = "ward.D2")
        ordered_samples <- hc$labels[hc$order]
        
        # Paruošiam faktorius duomenyse
        df$sample <- factor(df$sample, levels = ordered_samples)
        
        # B. Grafikų kūrimas
        p_bar <- create_base_barplot(df, cols, ordered_samples)
        p_tree <- create_dendro_plot(hc, ordered_samples)
        
        # C. Sujungimas
        plotly::subplot(p_bar, p_tree, nrows = 2, heights = c(0.8, 0.2),
                        shareX = FALSE, titleY = TRUE, margin = c(0.01, 0.01, 0.01, 0.08)) %>%
          plotly::layout(
            xaxis = list(         # barplot ašis (apačioje) – su pavadinimais
              tickangle = 90,
              tickmode = "array",
              tickvals = ordered_samples,
              ticktext = ordered_samples,
              showticklabels = TRUE
            ),
            xaxis2 = list(        # dendro ašis – be pavadinimų
              showticklabels = FALSE,
              categoryorder = "array",
              categoryarray = ordered_samples,
              matches = "x"
            ),
            margin = list(b = 150)
          )
        
      }, error = function(e) {
        message("Dendrogram error: ", e)
        # Fallback: jei dendrograma nepavyksta, braižom paprastą barplot
        create_base_barplot(df, cols) %>% 
          plotly::layout(title = "Dendrogram error - showing unsorted plot")
      })
    })
    
    output$plot_heatmap <- renderPlotly({
      req(processed_counts(), sample_order())
      mat <- as.matrix(processed_counts()$matrix)
      
      mat_rel <- sweep(mat, 1, rowSums(mat), "/")
      top_n_taxa <- names(sort(colMeans(mat_rel), decreasing = TRUE))[1:min(input$top_n, ncol(mat_rel))]
      
      if (input$sort_by != "dendro") {
        # --- Paprastas atvejis (be dendrogramos) ---
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
        # --- Dendrogramos atvejis ---
        tryCatch({
          # 1. Klasterizacija (ta pati logika kaip bar plote)
          dist_mat <- vegan::vegdist(mat, method = input$beta_metric)
          hc <- hclust(dist_mat, method = "ward.D2")
          ordered_samples <- hc$labels[hc$order]
          
          # 2. Paruošiame matricą tinkama tvarka
          avail_samples <- intersect(ordered_samples, rownames(mat_rel))
          mat_plot <- t(mat_rel[avail_samples, top_n_taxa, drop = FALSE])
          
          # 3. Heatmap grafikas
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
          
          # 4. Dendrograma (ta pati funkcija kaip bar plote)
          p_tree <- create_dendro_plot(hc, avail_samples)
          
          # 5. Subplot: dendrograma viršuje, heatmap apačioje
          plotly::subplot(
            p_heat, p_tree,
            nrows = 2,
            heights = c(0.8, 0.2),
            shareX = FALSE,
            titleY = TRUE,
            margin = c(0.01, 0.01, 0.01, 0.08)
          ) %>%
            plotly::layout(
              xaxis2 = list(          # dendro ašis – be pavadinimų
                showticklabels = FALSE,
                categoryorder = "array",
                categoryarray = avail_samples,
                matches = "x"
              ),
              xaxis = list(         # heatmap ašis – su pavadinimais
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
          # Fallback – paprastas heatmap be dendrogramos
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
  })
}