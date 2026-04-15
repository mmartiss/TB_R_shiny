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
      ),
      column(4,
             tags$label("Color palatte"),
             selectInput(ns("color_palette"), NULL,
                         choices = c("Tableau 10", "Pastel", "Bold", "Viridis-disc", "Earth"),
                         selected = "Tableau 10")
      ),
      column(8,
             tags$label("TAx color editor"),
             uiOutput(ns("color_editor"))
      )
    ),
    
    tabsetPanel(
      tabPanel("Taxonomic Bar Chart",       plotlyOutput(ns("plot_bar"),     height = "600px")),
      tabPanel("Relative Abundance Heatmap",plotlyOutput(ns("plot_heatmap"), height = "600px")),
      tabPanel("Alpha Diversity",           plotlyOutput(ns("plot_alpha"),   height = "500px")),
      tabPanel("Beta Diversity (PCoA)",     plotlyOutput(ns("plot_beta"),    height = "500px"))
    )
  )
}

ampliconServer <- function(id, abundance, samples) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    TAX_COLS <- c("tax_id", "species", "genus", "family", "order", "class",
                  "phylum", "clade", "superkingdom", "subspecies",
                  "species.subgroup", "species.group", "estimated.counts", "abundance")
    
    # ── Paletės ───────────────────────────────────────────────────────────────
    
    PALETTES <- list(
      "Tableau 10"   = c("#4E79A7","#F28E2B","#E15759","#76B7B2","#59A14F",
                         "#EDC948","#B07AA1","#FF9DA7","#9C755F","#BAB0AC"),
      "Pastel"       = c("#AEC6CF","#FFD1DC","#B5EAD7","#FFDAC1","#C7CEEA",
                         "#E2F0CB","#F2D7D9","#D5E8D4","#DAE8FC","#F8CECC"),
      "Bold"         = c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00",
                         "#A65628","#F781BF","#999999","#66C2A5","#FC8D62"),
      "Viridis-disc" = c("#440154","#30678D","#35B779","#FDE724","#482677",
                         "#1F9E89","#73D055","#DCE319","#3CBB75","#94D840"),
      "Earth"        = c("#8C510A","#BF812D","#DFC27D","#80CDC1","#35978F",
                         "#01665E","#543005","#F6E8C3","#C7EAE5","#003C30")
    )
    
    # ── Spalvų reaktyvios reikšmės ────────────────────────────────────────────
    
    rv_colors <- reactiveValues(map = list())
    
    # ── Duomenų paruošimas (tik po mygtuko) ───────────────────────────────────
    
    prepared <- eventReactive(input$btn_generate, {
      ab  <- abundance()
      smp <- samples()
      
      names(ab) <- gsub("\\.fastq$", "", names(ab))
      
      #cia jei galunes butu ivairios
      #names(ab) <- tools::file_path_sans_ext(names(ab))
      
      if (!"tax_id" %in% names(ab) || !"tax_id" %in% names(smp)) {
        showNotification("Error: Both files must contain a 'tax_id' column for the merge.", type = "error")
        return(NULL)
      }
      
      ab_tax <- merge(ab, smp, by = "tax_id", all.x = TRUE)
      
      level <- input$tax_level
      if (!level %in% names(ab_tax)) {
        showNotification(paste("Column", level, "not found in the sample data."), type = "error")
        return(NULL)
      }
      
      sample_cols <- setdiff(names(ab), "tax_id")
      sample_cols <- sample_cols[sapply(ab[sample_cols], is.numeric)]
      
      ab_tax[[level]][is.na(ab_tax[[level]]) | ab_tax[[level]] == ""] <- paste0("Unknown_", level)
      
      long_list <- lapply(sample_cols, function(sc) {
        data.frame(
          sample    = sc,
          taxon     = as.character(ab_tax[[level]]),
          abundance = as.numeric(ab_tax[[sc]]),
          stringsAsFactors = FALSE
        )
      })
      long <- do.call(rbind, long_list)
      
      long_agg <- aggregate(abundance ~ sample + taxon, data = long, FUN = sum, na.rm = TRUE)
      
      long_rel_full <- do.call(rbind, lapply(split(long_agg, long_agg$sample), function(s) {
        total <- sum(s$abundance, na.rm = TRUE)
        s$rel <- if (total > 0) s$abundance / total else 0
        s
      }))
      
      taxon_means <- aggregate(rel ~ taxon, data = long_rel_full, FUN = mean)
      top_taxa    <- taxon_means$taxon[order(taxon_means$rel, decreasing = TRUE)][1:min(input$top_n, nrow(taxon_means))]
      
      final_df <- long_rel_full[long_rel_full$taxon %in% top_taxa & long_rel_full$rel >= input$min_abund, ]
      
      list(
        long_filt   = final_df,
        long_full   = long_rel_full,
        sample_cols = sample_cols,
        level       = level
      )
    })
    
    # ── Aktyvūs taksai ────────────────────────────────────────────────────────
    
    active_taxa <- reactive({
      req(prepared())
      unique(prepared()$long_filt$taxon)
    })
    
    # ── Kai pasikeičia paletė arba taksai — atnaujink rv_colors ──────────────
    
    observe({
      req(active_taxa())
      taxa <- active_taxa()
      pal  <- PALETTES[[input$color_palette]]
      cols <- rep_len(pal, length(taxa))
      rv_colors$map <- setNames(as.list(cols), taxa)
    })
    
    # ── Color editor UI ───────────────────────────────────────────────────────
    
    output$color_editor <- renderUI({
      req(active_taxa(), rv_colors$map)
      taxa <- active_taxa()
      
      tagList(
        div(style = "display:flex; flex-wrap:wrap; gap:8px; margin-top:4px;",
            lapply(taxa, function(t) {
              input_id <- paste0("col_", gsub("[^A-Za-z0-9]", "_", t))
              div(style = "display:flex; align-items:center; gap:4px;",
                  tags$input(
                    type  = "color",
                    id    = ns(input_id),
                    value = rv_colors$map[[t]],
                    onchange = sprintf("Shiny.setInputValue('%s', this.value)", ns(input_id)),
                    style = "width:28px; height:28px; border:none; border-radius:50%%; cursor:pointer; padding:0;"
                  ),
                  tags$small(t, style = "font-size:11px;")
              )
            })
        )
      )
    })
    
    # ── Stebėk kiekvieno color picker'io pasikeitimą ──────────────────────────
    
    observe({
      req(active_taxa())
      taxa <- active_taxa()
      
      lapply(taxa, function(t) {
        input_id <- paste0("col_", gsub("[^A-Za-z0-9]", "_", t))
        observeEvent(input[[input_id]], {
          rv_colors$map[[t]] <- input[[input_id]]
        }, ignoreInit = TRUE)
      })
    })
    
    # ── Galutinis spalvų žemėlapis ────────────────────────────────────────────
    
    color_map <- reactive({
      req(rv_colors$map)
      unlist(rv_colors$map)
    })
    
    # ── Bar chart ─────────────────────────────────────────────────────────────
    
    output$plot_bar <- renderPlotly({
      req(prepared(), color_map())
      df         <- prepared()$long_filt
      palette    <- color_map()
      
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
      df <- prepared()$long_filt
      
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
      df_full <- prepared()$long_full
      
      alpha_df <- do.call(rbind, lapply(split(df_full, df_full$sample), function(d) {
        p <- d$rel[d$rel > 0]
        if (length(p) == 0) return(NULL)
        data.frame(
          sample   = unique(d$sample),
          Shannon  = -sum(p * log(p)),
          Simpson  = 1 - sum(p^2),
          Observed = length(p),
          stringsAsFactors = FALSE
        )
      }))
      
      p1 <- plot_ly(alpha_df, y = ~Observed, type = "box", name = "Observed",
                    boxpoints = "all", jitter = 0.3, text = ~sample,
                    marker = list(color = "#4E79A7")) %>%
        layout(yaxis = list(title = "Count"))
      
      p2 <- plot_ly(alpha_df, y = ~Shannon, type = "box", name = "Shannon",
                    boxpoints = "all", jitter = 0.3, text = ~sample,
                    marker = list(color = "#F28E2B")) %>%
        layout(yaxis = list(title = "Index H'"))
      
      p3 <- plot_ly(alpha_df, y = ~Simpson, type = "box", name = "Simpson",
                    boxpoints = "all", jitter = 0.3, text = ~sample,
                    marker = list(color = "#E15759")) %>%
        layout(yaxis = list(title = "Index 1-D"))
      
      subplot(p1, p2, p3, nrows = 1, margin = 0.05, titleY = TRUE) %>%
        layout(
          title      = list(text = "Alpha Diversity Metrics", x = 0),
          showlegend = FALSE,
          margin     = list(t = 50, b = 50)
        )
    })
    
    # ── Beta diversity (PCoA) ─────────────────────────────────────────────────
    
    output$plot_beta <- renderPlotly({
      req(prepared())
      df_full <- prepared()$long_full
      
      mat <- reshape(df_full[, c("sample", "taxon", "rel")],
                     idvar     = "sample",
                     timevar   = "taxon",
                     direction = "wide")
      rownames(mat) <- mat$sample
      mat$sample    <- NULL
      mat[is.na(mat)] <- 0
      
      if (nrow(mat) < 3) {
        return(plot_ly() |> layout(title = "Need at least 3 samples for PCoA"))
      }
      
      dist_mat <- dist(as.matrix(mat), method = "euclidean")
      pcoa_res <- cmdscale(dist_mat, k = 2, eig = TRUE)
      var_exp  <- round(pcoa_res$eig / sum(pcoa_res$eig) * 100, 1)
      
      pcoa_df <- data.frame(
        sample = rownames(mat),
        PC1    = pcoa_res$points[, 1],
        PC2    = pcoa_res$points[, 2]
      )
      
      plot_ly(pcoa_df,
              x             = ~PC1,
              y             = ~PC2,
              text          = ~sample,
              type          = "scatter",
              mode          = "markers+text",
              textposition  = "top center",
              marker        = list(size = 12, opacity = 0.7),
              hovertemplate = "<b>%{text}</b><br>PC1: %{x:.3f}<br>PC2: %{y:.3f}<extra></extra>"
      ) |>
        layout(
          xaxis = list(title = paste0("PC1 (", var_exp[1], "%)")),
          yaxis = list(title = paste0("PC2 (", var_exp[2], "%)"))
        )
    })
    
  })
}