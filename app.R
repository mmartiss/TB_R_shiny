# ============================================================
#  app.R — Entry point
#  Source order matters: helpers → modules → ui → server
# ============================================================

library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(ggplot2)

source("R/helpers.R")
source("R/modules/mod_upload.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
