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
              h2("Pradzia")
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
              ampliconUI("ampliconID")
      )
    )
  ) 
)