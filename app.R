# ============================================================
#  app.R — Entry point
# ============================================================

library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(ggplot2)

source("R/helpers.R")

source("R/modules/mod_upload.R")
#source("R/modules/mod_qc.R")
#source("R/modules/mod_filter.R")
#source("R/modules/mod_annotation.R")
#source("R/modules/mod_visualization.R")

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
