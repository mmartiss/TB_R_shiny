ampliconUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Amplicon Analysis (Relative Abundance)"),
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
             tags$label("Min. abundance"),
             numericInput(ns("min_abund"), NULL, value = 0.0001, min = 0, max = 1, step = 0.001)
      ),
      column(2,
             tags$label("Color palette"),
             selectInput(ns("color_palette"), NULL,
                         choices = c("Tableau 10", "Pastel", "Bold", "Viridis-disc", "Earth"),
                         selected = "Tableau 10")
      ),
      column(3,
             tags$br(),
             actionButton(ns("btn_generate"), "Update Plots", class = "btn-success w-100")
      )
    ),
    hr(),
    uiOutput(ns("color_editor_ui")),
    hr(),
    tabsetPanel(
      tabPanel("Taxonomic Bar Chart", plotlyOutput(ns("plot_bar"), height = "600px")),
      tabPanel("Relative Abundance Heatmap", plotlyOutput(ns("plot_heatmap"), height = "600px"))
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
    
    prepared_data <- eventReactive(input$btn_generate, {
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
      
      # 4. Agreguojame (Sumuojame rūšis į Genus/Family/t.t.)
      long_df <- merged %>%
        dplyr::select(dplyr::all_of(c(level, sample_cols))) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(sample_cols), names_to = "sample", values_to = "abund") %>%
        # Išvalome pavadinimus
        dplyr::mutate(sample = gsub("\\.fastq$|\\.fq$|\\.fastq\\.gz$|\\.fastq-threshold-.*$", "", sample)) %>%
        # Grupuojame pagal mėginį ir pasirinktą lygį
        dplyr::group_by(sample, taxon = .data[[level]]) %>%
        dplyr::summarise(abundance = sum(abund, na.rm = TRUE), .groups = "drop")
      
      # 5. Randame TOP N taksonus (pagal vidurkį visuose mėginiuose)
      top_taxa <- long_df %>%
        dplyr::group_by(taxon) %>%
        dplyr::summarise(mean_val = mean(abundance)) %>%
        dplyr::filter(taxon != "Unknown") %>%
        dplyr::arrange(desc(mean_val)) %>%
        dplyr::slice_head(n = input$top_n) %>%
        dplyr::pull(taxon)
      
      # 6. Finalinis filtravimas: 
      # Viskas, kas ne TOP N ir nesiekia min_abund, keliauja į "Other"
      final_df <- long_df %>%
        dplyr::mutate(taxon = ifelse(taxon %in% top_taxa & abundance >= input$min_abund, taxon, "Other")) %>%
        dplyr::group_by(sample, taxon) %>%
        dplyr::summarise(rel_abund = sum(abundance), .groups = "drop")
      
      # 7. Užtikriname, kad stulpeliai būtų 100% (normalizacija po grupavimo)
      final_df <- final_df %>%
        dplyr::group_by(sample) %>%
        dplyr::mutate(rel_abund = rel_abund / sum(rel_abund)) %>%
        dplyr::ungroup()
      
      list(df = final_df, level = level)
    })
    
    observe({
      req(prepared_data())
      taxa <- unique(prepared_data()$df$taxon)
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
      tags$div(style = "display:flex; flex-wrap:wrap; gap:10px; padding: 10px; background: #f9f9f9; border-radius: 5px;",
               lapply(taxa, function(t) {
                 input_id <- paste0("col_", gsub("[^A-Za-z0-9]", "_", t))
                 tags$div(style = "display:flex; align-items:center; gap:5px; border: 1px solid #ddd; padding: 2px 5px; border-radius: 3px;",
                          tags$input(type = "color", 
                                     value = rv_colors$map[[t]],
                                     onchange = sprintf("Shiny.setInputValue('%s', this.value)", ns(input_id)),
                                     style="width:25px; height:25px; border:none; cursor:pointer;"),
                          tags$span(t, style="font-size:11px; font-weight: bold;")
                 )
               })
      )
    })
    
    observe({
      req(rv_colors$map)
      taxa <- names(rv_colors$map)
      for (t in taxa) {
        input_id <- paste0("col_", gsub("[^A-Za-z0-9]", "_", t))
        val <- input[[input_id]]
        if (!is.null(val)) {
          rv_colors$map[[t]] <- val
        }
      }
    })
    
    output$plot_bar <- renderPlotly({
      req(prepared_data(), rv_colors$map)
      df <- prepared_data()$df
      cols <- unlist(rv_colors$map)
      
      # Užtikriname taksonų eiliškumą, kad "Other" būtų apačioje arba viršuje
      df$taxon <- factor(df$taxon, levels = names(cols))
      
      plotly::plot_ly(df, x = ~sample, y = ~rel_abund, color = ~taxon, colors = cols,
                      type = "bar", text = ~paste0(taxon, ": ", round(rel_abund*100, 2), "%"),
                      hoverinfo = "text") %>%
        plotly::layout(barmode = "stack",
                       yaxis = list(title = "Relative Abundance", tickformat = ".0%"),
                       xaxis = list(title = "Samples", tickangle = -45),
                       showlegend = TRUE,
                       margin = list(b = 100))
    })
    
    output$plot_heatmap <- renderPlotly({
      req(prepared_data())
      df <- prepared_data()$df
      
      mat_df <- df %>%
        tidyr::pivot_wider(names_from = sample, values_from = rel_abund, values_fill = 0)
      
      mat <- as.matrix(mat_df[,-1])
      rownames(mat) <- mat_df$taxon
      
      plotly::plot_ly(x = colnames(mat), y = rownames(mat), z = mat, 
                      type = "heatmap", colorscale = "Viridis") %>%
        plotly::layout(xaxis = list(tickangle = -45), 
                       yaxis = list(title = ""),
                       margin = list(b = 100, l = 150))
    })
  })
}