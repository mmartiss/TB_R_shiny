# ============================================================
#  R/helpers.R — Shared UI constructors + Plotly utilities
#  Source this in app.R before modules
# ============================================================

page_wrap <- function(title, subtitle, ...) {
  div(class = "pg-inner",
      div(class = "pg-head",
          tags$h2(class = "pg-title", tolower(title)),
          tags$p(class  = "pg-sub",   subtitle)
      ),
      ...
  )
}

two_col <- function(...) div(class = "two-col", ...)

card <- function(...) div(class = "card", ...)

card_full <- function(...) div(class = "card card-full", ...)

card_label <- function(text) div(class = "clabel", text)

kv_row <- function(output_id, label) {
  div(class = "kv",
      div(class = "kv-label", label),
      div(class = "kv-value", textOutput(output_id, inline = TRUE))
  )
}

minimal_layout <- function(p, xlab = "", ylab = "") {
  p %>% plotly::layout(
    paper_bgcolor = "transparent",
    plot_bgcolor  = "transparent",
    font   = list(color = "#aaa", family = "DM Mono", size = 11),
    xaxis  = list(title = xlab, gridcolor = "#1e1e1e",
                  zerolinecolor = "#1e1e1e", tickfont = list(size = 10)),
    yaxis  = list(title = ylab, gridcolor = "#1e1e1e",
                  zerolinecolor = "#1e1e1e", tickfont = list(size = 10)),
    margin = list(t = 10, b = 40, l = 50, r = 10)
  )
}

empty_plotly <- function(msg = "No data") {
  plotly::plot_ly() %>%
    plotly::layout(
      paper_bgcolor = "transparent",
      plot_bgcolor  = "transparent",
      annotations   = list(list(
        text      = msg, showarrow = FALSE,
        font      = list(color = "#444", size = 13, family = "DM Mono"),
        xref      = "paper", yref = "paper", x = 0.5, y = 0.5
      )),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}
