# ============================================================
#  ui.R
#  CSS is read from R/theme.R (plain CSS text) and inlined
#  via tags$style(HTML(...)) — no www/ dependency
# ============================================================

.css <- paste(readLines("R/theme.R", warn = FALSE), collapse = "\n")

ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$title("variantR"),
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=DM+Mono:ital,wght@0,300;0,400;0,500;1,300&family=Syne:wght@400;700&display=swap"
    ),
    tags$style(HTML(.css))
  ),

  div(class = "shell",

      tags$nav(class = "rail",
               div(class = "rail-wordmark", "variantR"),
               tags$ul(class = "rail-nav",
                       # ADDED THE OTHER NAV ITEMS HERE
                       rail_item("nav_upload", "01", "upload", active = TRUE),
                       rail_item("nav_qc",     "02", "quality control"),
                       rail_item("nav_filter", "03", "filtering"),
                       rail_item("nav_annot",  "04", "annotation"),
                       rail_item("nav_viz",    "05", "visualization")
               ),
               div(class = "rail-bottom",
                   div(class = "pipe-dots", uiOutput("pipeline_dots")),
                   div(class = "rail-ver", "v0.1.0")
               )
      ),
      
      div(class = "content",
          div(class = "strip",
              div(class = "strip-path",
                  tags$span(class = "strip-root", "files"),
                  tags$span(class = "strip-arrow", "\u203a"),
                  textOutput("strip_title", inline = TRUE)
              ),
              div(class = "strip-right",
                  uiOutput("step_status"),
                  actionButton("btn_run", "run", class = "run-btn")
              )
          ),
          div(class = "pages",
              # ADDED THE OTHER PAGE DIVS HERE
              div(id = "pg_upload", class = "pg active", mod_upload_ui("upload")),
              div(id = "pg_qc",     class = "pg",        mod_qc_ui("qc")),
              div(id = "pg_filter", class = "pg",        mod_filter_ui("filter")),
              div(id = "pg_annot",  class = "pg",        mod_annotation_ui("annot")),
              div(id = "pg_viz",    class = "pg",        mod_visualization_ui("viz"))
          )
      )
    )
)
