setwd("~/MartynasLib/Universitetas/4 kursas/8_semestras/praktika/mano_r")

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(ape)
library(readxl)

source("modules/upload.r")
source("modules/filter.r")
source("modules/amplicon.r")

source("ui.r")
source("server.r")

shinyApp(ui, server)