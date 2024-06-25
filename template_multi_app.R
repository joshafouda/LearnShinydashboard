library(shinydashboard)
library(shiny)

header <- dashboardHeader(title = "Template")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "pages",
    menuItem("Many charts",
             tabName = "charts"),
    menuItem("Statistics",
             tabName = "stats"),
    menuItem("A couple of checkboxes"),
    menuItem("Overall results", tabName = "overall", 
             # Add two subtabs called "Charts" and "Data table"
             menuSubItem("Charts", tabName = "charts"),
             menuSubItem("Data table", tabName = "datatable", icon=icon("file-excel")))
  )
)

body <- dashboardBody()

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
}

shinyApp(ui, server)