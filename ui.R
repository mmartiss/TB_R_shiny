ui <- dashboardPage(
  skin = "black",
  title = "TB",
  
  #headeris
  dashboardHeader(
    title = span("TB"),
    titleWidth = 100
  ),
  #sidebar
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Start", tabName = "start"),
      menuItem("File upload", tabName = "upload"),
      menuItem("Filtering", tabName = "filter"),
      menuItem("Amplicon", tabName = "amplicon")
    )
  ),
  #main
  dashboardBody(
    tabItems(
      tabItem(tabName = "start",
              h2("Pradzia")
      ),
      tabItem(
        tabName = "upload", 
        uploadUI("uploadID"),
        verbatimTextOutput("debug")
      ),
      tabItem(tabName = "filter",
              filterUI("filterID")
      ),
      tabItem(tabName = "amplicon",
              ampliconUI("ampliconID")
      )
    )
  ) 
)