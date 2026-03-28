# ============================================================
#  ui.R — Application UI
# ============================================================

ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$link(rel  = "stylesheet", href = "css/theme.css"),
    tags$title("variantR")
  ),

  div(class = "shell",

    tags$nav(class = "rail",

      div(class = "rail-wordmark", "variantR"),

      tags$ul(class = "rail-nav",
        rail_item("nav_upload", "01", "upload",     active = TRUE),
#        rail_item("nav_qc",     "02", "qc"),
#        rail_item("nav_filter", "03", "filter"),
#        rail_item("nav_annot",  "04", "annotate"),
#        rail_item("nav_viz",    "05", "visualize")
      ),

      div(class = "rail-bottom",
        div(class = "pipeline-indicator",
          uiOutput("pipeline_dots")
        ),
        div(class = "rail-version", "v0.1.0")
      )
    ),

    div(class = "content",

      div(class = "strip",
        div(class = "strip-path",
          tags$span(class = "strip-root", "vcf"),
          tags$span(class = "strip-arrow", "›"),
          textOutput("strip_title", inline = TRUE)
        ),
        div(class = "strip-right",
          uiOutput("step_status"),
          actionButton("btn_run", "run", class = "run-btn")
        )
      ),

      div(class = "pages",
        div(id = "pg_upload", class = "pg active", mod_upload_ui("upload")),
#        div(id = "pg_qc",     class = "pg",        mod_qc_ui("qc")),
#        div(id = "pg_filter", class = "pg",        mod_filter_ui("filter")),
#        div(id = "pg_annot",  class = "pg",        mod_annotation_ui("annot")),
#        div(id = "pg_viz",    class = "pg",        mod_visualization_ui("viz"))
      )
    )
  )
)

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
