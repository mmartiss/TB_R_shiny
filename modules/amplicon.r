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
      column(3,
             tags$label("Top N taxa"),
             numericInput(ns("top_n"), NULL, value = 20, min = 5, max = 100, step = 5)
      ),
      column(3,
             tags$label("Min. abundance threshold"),
             numericInput(ns("min_abund"), NULL, value = 0.001, min = 0, max = 1, step = 0.001)
      ),
      column(3,
             tags$br(),
             actionButton(ns("btn_generate"), "Generate plots", class = "btn-success")
      )
    ),
    
    tabsetPanel(
      tabPanel("Taxonomic Bar Chart",       plotlyOutput(ns("plot_bar"),     height = "600px")),
      tabPanel("Relative Abundance Heatmap",plotlyOutput(ns("plot_heatmap"), height = "600px")),
      tabPanel("Alpha Diversity",           plotlyOutput(ns("plot_alpha"),   height = "500px")),
      tabPanel("Beta Diversity (PCoA)",     plotlyOutput(ns("plot_beta"),    height = "500px")),
      tabPanel("Rarefaction Curves",        plotlyOutput(ns("plot_rare"),    height = "500px"))
    )
  )
}

ampliconServer <- function(id, abundance, samples) {
  moduleServer(id, function(input, output, session) {
    
    TAX_COLS <- c("tax_id", "species", "genus", "family", "order", "class",
                  "phylum", "clade", "superkingdom", "subspecies",
                  "species.subgroup", "species.group", "estimated.counts", "abundance")
    
    # ── Duomenų paruošimas (tik po mygtuko) ───────────────────────────────────
    
    prepared <- eventReactive(input$btn_generate, {
      req(abundance(), samples())
      
      ab  <- abundance()
      smp <- samples()
      
      # Abundance wide: tax_id + mėginių stulpeliai
      sample_cols <- setdiff(names(ab), "tax_id")
      
      # Taxonomy lookup iš samples failo
      tax_lookup <- smp[, intersect(names(smp), TAX_COLS), drop = FALSE]
      
      # Sujungti taxonomy į abundance
      ab_tax <- merge(ab, tax_lookup, by = "tax_id", all.x = TRUE)
      
      level <- input$tax_level
      
      if (!level %in% names(ab_tax)) {
        showNotification(paste("Stulpelis", level, "nerastas duomenyse."), type = "error")
        return(NULL)
      }
      
      ab_tax[[level]] <- ifelse(
        is.na(ab_tax[[level]]) | ab_tax[[level]] == "",
        paste0("Unknown_", level),
        ab_tax[[level]]
      )
      
      # Agreguoti pagal taxonominį lygį -> long format
      long <- do.call(rbind, lapply(sample_cols, function(sc) {
        data.frame(
          sample    = sc,
          taxon     = ab_tax[[level]],
          abundance = as.numeric(ab_tax[[sc]]),
          stringsAsFactors = FALSE
        )
      }))
      
      long <- long[!is.na(long$abundance), ]
      
      # Normalizuoti kiekvienam mėginiui
      long <- do.call(rbind, lapply(split(long, long$sample), function(s) {
        agg        <- aggregate(abundance ~ taxon, data = s, FUN = sum, na.rm = TRUE)
        total      <- sum(agg$abundance, na.rm = TRUE)
        agg$rel    <- if (total > 0) agg$abundance / total else agg$abundance
        agg$sample <- unique(s$sample)
        agg
      }))
      
      # Top N taxa
      taxon_means <- tapply(long$rel, long$taxon, mean, na.rm = TRUE)
      top_taxa    <- names(sort(taxon_means, decreasing = TRUE))[seq_len(min(input$top_n, length(taxon_means)))]
      
      long <- long[long$taxon %in% top_taxa & long$rel >= input$min_abund, ]
      
      # Gražinti taip pat counts iš samples (rarefaction + alpha)
      list(
        long       = long,
        ab_tax     = ab_tax,
        sample_cols = sample_cols,
        smp        = smp,
        level      = level
      )
    })
    
    # ── Bar chart ─────────────────────────────────────────────────────────────
    
    output$plot_bar <- renderPlotly({
      req(prepared())
      df      <- prepared()$long
      n_taxa  <- length(unique(df$taxon))
      palette <- colorRampPalette(c(
        "#4E79A7","#F28E2B","#E15759","#76B7B2","#59A14F",
        "#EDC948","#B07AA1","#FF9DA7","#9C755F","#BAB0AC"
      ))(n_taxa)
      
      plot_ly(df,
              x         = ~sample,
              y         = ~rel,
              color     = ~taxon,
              colors    = palette,
              type      = "bar",
              text      = ~paste0(taxon, "<br>", round(rel * 100, 2), "%"),
              hoverinfo = "text"
      ) |>
        layout(
          barmode = "stack",
          xaxis   = list(title = "Sample", tickangle = -45),
          yaxis   = list(title = "Relative Abundance", tickformat = ".0%"),
          legend  = list(title = list(text = prepared()$level)),
          margin  = list(b = 120)
        )
    })
    
    # ── Heatmap ───────────────────────────────────────────────────────────────
    
    output$plot_heatmap <- renderPlotly({
      req(prepared())
      df <- prepared()$long
      
      mat <- reshape(df[, c("sample", "taxon", "rel")],
                     idvar     = "taxon",
                     timevar   = "sample",
                     direction = "wide")
      rownames(mat) <- mat$taxon
      mat$taxon     <- NULL
      names(mat)    <- gsub("^rel\\.", "", names(mat))
      mat[is.na(mat)] <- 0
      
      plot_ly(
        x             = colnames(mat),
        y             = rownames(mat),
        z             = as.matrix(mat),
        type          = "heatmap",
        colorscale    = "Viridis",
        hovertemplate = "Sample: %{x}<br>Taxon: %{y}<br>Rel. abundance: %{z:.3f}<extra></extra>"
      ) |>
        layout(
          xaxis  = list(title = "Sample", tickangle = -45),
          yaxis  = list(title = prepared()$level, automargin = TRUE),
          margin = list(b = 120, l = 180)
        )
    })
    
    # ── Alpha diversity ───────────────────────────────────────────────────────
    
    output$plot_alpha <- renderPlotly({
      req(prepared())
      ab_tax      <- prepared()$ab_tax
      sample_cols <- prepared()$sample_cols
      
      alpha_df <- do.call(rbind, lapply(sample_cols, function(sc) {
        counts <- as.numeric(ab_tax[[sc]])
        counts <- counts[!is.na(counts) & counts > 0]
        if (length(counts) == 0) return(NULL)
        
        p        <- counts / sum(counts)
        shannon  <- -sum(p * log(p))
        simpson  <- 1 - sum(p^2)
        observed <- length(counts)
        
        data.frame(sample = sc, Shannon = shannon, Simpson = simpson, Observed = observed)
      }))
      
      df_long <- rbind(
        data.frame(sample = alpha_df$sample, metric = "Shannon",  value = alpha_df$Shannon),
        data.frame(sample = alpha_df$sample, metric = "Simpson",  value = alpha_df$Simpson),
        data.frame(sample = alpha_df$sample, metric = "Observed", value = alpha_df$Observed)
      )
      
      plot_ly(df_long,
              x         = ~metric,
              y         = ~value,
              color     = ~metric,
              type      = "box",
              boxpoints = "all",
              jitter    = 0.3,
              text      = ~paste0(sample, "<br>", round(value, 3)),
              hoverinfo = "text"
      ) |>
        layout(
          xaxis  = list(title = "Metric"),
          yaxis  = list(title = "Value"),
          legend = list(title = list(text = "Metric"))
        )
    })
    
    # ── Beta diversity (PCoA) ─────────────────────────────────────────────────
    
    output$plot_beta <- renderPlotly({
      req(prepared())
      df <- prepared()$long
      
      mat <- reshape(df[, c("sample", "taxon", "rel")],
                     idvar     = "taxon",
                     timevar   = "sample",
                     direction = "wide")
      rownames(mat) <- mat$taxon
      mat$taxon     <- NULL
      names(mat)    <- gsub("^rel\\.", "", names(mat))
      mat[is.na(mat)] <- 0
      
      mat_t <- t(as.matrix(mat))
      
      if (nrow(mat_t) < 3) {
        return(plot_ly() |> layout(title = "Reikia bent 3 mėginių PCoA"))
      }
      
      dist_mat <- as.matrix(dist(mat_t, method = "euclidean"))
      n        <- nrow(dist_mat)
      H        <- diag(n) - matrix(1/n, n, n)
      B        <- -0.5 * H %*% (dist_mat^2) %*% H
      eig      <- eigen(B, symmetric = TRUE)
      
      pos_eig  <- eig$values[eig$values > 1e-10]
      coords   <- eig$vectors[, 1:2] %*% diag(sqrt(pmax(eig$values[1:2], 0)))
      var_exp  <- round(eig$values[1:2] / sum(pos_eig) * 100, 1)
      
      pcoa_df  <- data.frame(
        sample = rownames(mat_t),
        PC1    = coords[, 1],
        PC2    = coords[, 2]
      )
      
      plot_ly(pcoa_df,
              x            = ~PC1,
              y            = ~PC2,
              text         = ~sample,
              type         = "scatter",
              mode         = "markers+text",
              textposition = "top center",
              marker       = list(size = 10),
              hovertemplate = "%{text}<br>PC1: %{x:.3f}<br>PC2: %{y:.3f}<extra></extra>"
      ) |>
        layout(
          xaxis = list(title = paste0("PC1 (", var_exp[1], "%)")),
          yaxis = list(title = paste0("PC2 (", var_exp[2], "%)"))
        )
    })
    
    # ── Rarefaction curves ────────────────────────────────────────────────────
    
    output$plot_rare <- renderPlotly({
      req(prepared())
      ab_tax      <- prepared()$ab_tax
      sample_cols <- prepared()$sample_cols
      
      rare_data <- do.call(rbind, lapply(sample_cols, function(sc) {
        counts <- round(as.numeric(ab_tax[[sc]]))
        counts <- counts[!is.na(counts) & counts > 0]
        if (length(counts) == 0) return(NULL)
        
        total  <- sum(counts)
        steps  <- unique(round(seq(0, total, length.out = 20)))
        steps  <- steps[steps > 0]
        
        richness <- sapply(steps, function(n) {
          if (n >= total) return(sum(counts > 0))
          prob_absent <- sapply(counts, function(c) {
            lc  <- lchoose(total - c, n)
            lt  <- lchoose(total, n)
            if (is.nan(lc) || is.infinite(lc)) return(1)
            exp(lc - lt)
          })
          sum(1 - prob_absent)
        })
        
        data.frame(sample = sc, depth = steps, richness = richness)
      }))
      
      if (is.null(rare_data)) {
        return(plot_ly() |> layout(title = "Nepavyko apskaičiuoti rarefaction kreivių"))
      }
      
      plot_ly(rare_data,
              x             = ~depth,
              y             = ~richness,
              color         = ~sample,
              type          = "scatter",
              mode          = "lines",
              hovertemplate = "Sample: %{fullData.name}<br>Depth: %{x}<br>Richness: %{y:.0f}<extra></extra>"
      ) |>
        layout(
          xaxis  = list(title = "Sequencing depth"),
          yaxis  = list(title = "Observed taxa"),
          legend = list(title = list(text = "Sample"))
        )
    })
    
  })
}