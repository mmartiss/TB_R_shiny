# ============================================================
#  R/helpers.R — UI constructors + plotly utils
#  source() this FIRST in app.R
# ============================================================

# Nav
rail_item <- function(id, num, label, active = FALSE) {
  tags$li(
    actionButton(id,
      label = tagList(
        tags$span(class = "ri-num", num),
        tags$span(class = "ri-label", label)
      ),
      class = paste("ri", if (active) "active")
    )
  )
}

#Page wrapper
page_wrap <- function(title, subtitle, ...) {
  div(class = "pg-inner",
    div(class = "pg-head",
      tags$h2(class = "pg-title", title),
      tags$p(class  = "pg-sub",   subtitle)
    ),
    ...
  )
}

# layout
two_col   <- function(...) div(class = "two-col", ...)
card      <- function(...) div(class = "card", ...)
card_full <- function(...) div(class = "card card-full", ...)
clabel    <- function(text) div(class = "clabel", text)

# Key-value display row
kv_row <- function(output_id, label) {
  div(class = "kv",
    div(class = "kv-label", label),
    div(class = "kv-value", textOutput(output_id, inline = TRUE))
  )
}

# Plotly layout
dark_layout <- function(p, xlab = "", ylab = "") {
  p %>% plotly::layout(
    paper_bgcolor = "transparent",
    plot_bgcolor  = "transparent",
    font   = list(color = "#8496a9", family = "DM Mono", size = 11),
    xaxis  = list(title      = xlab,
                  gridcolor  = "#1c2433",
                  zerolinecolor = "#1c2433",
                  tickfont   = list(size = 10)),
    yaxis  = list(title      = ylab,
                  gridcolor  = "#1c2433",
                  zerolinecolor = "#1c2433",
                  tickfont   = list(size = 10)),
    margin = list(t = 10, b = 44, l = 54, r = 10)
  )
}

# Empty state plot
empty_plot <- function(msg = "No data loaded") {
  plotly::plot_ly() %>%
    plotly::layout(
      paper_bgcolor = "transparent",
      plot_bgcolor  = "transparent",
      annotations   = list(list(
        text      = msg,
        showarrow = FALSE,
        font      = list(color = "#3d4f63", size = 13, family = "DM Mono"),
        xref = "paper", yref = "paper", x = .5, y = .5
      )),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}
