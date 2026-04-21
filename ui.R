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
      menuItem("1. File upload", tabName = "upload"),
      menuItem("2. Abundance filtering", tabName = "abundance"),
      menuItem("3. Counts filtering", tabName = "counts"),
      menuItem("4. Taxonomy filtering", tabName = "taxonomy"),
      menuItem("5. Metadata filtering", tabName = "metadata"),
      menuItem("6. Amplicon", tabName = "amplicon")
    )
  ),
  #main
  dashboardBody(
    tabItems(
      tabItem(tabName = "start",
              fluidRow(
                box(
                  width = 12, 
                  status = "primary", 
                  solidHeader = TRUE,
                  
                  tags$head(
                    tags$style(HTML("
          table {
            border-collapse: collapse;
            width: 100%;
          }
          th, td {
            border: 1px solid #ddd;
            padding: 6px 10px;
            text-align: left;
            vertical-align: top;
          }
          th {
            background-color: #f2f2f2;
            font-weight: bold;
          }
          td {
            white-space: normal;
            word-wrap: break-word;
          }
        "))
                  ),
                  
                  includeMarkdown("README.md")
                )
              )
      ),
      tabItem(
        tabName = "upload", 
        uploadUI("uploadID")
      ),
      tabItem(
        tabName = "abundance",
        filterUI("ab_filt", "Abundance Filtering")
      ),
      tabItem(
        tabName = "counts",
        filterUI("counts_filt", "Counts Filtering")
      ),
      tabItem(
        tabName = "taxonomy",
        filterUI("tax_filt", "Taxonomy Filtering")
      ),
      tabItem(
        tabName = "metadata",
        metadataFilterUI("meta_filt")
      ),
      tabItem(tabName = "amplicon",
              ampliconUI("amplicon_1")
      )
    )
  ) 
)