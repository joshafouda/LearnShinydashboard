set.seed(123) # For reproducibility

# Load necessary libraries
library(dplyr)
library(lubridate)

# Simulating the dataframe
listings <- tibble(
  id = sample(1:1000000, 69351, replace = FALSE),
  name = sample(c("Lovely large room, Bethnal Green", "Holiday London DB Room Let-on going", "COSY STUDIO-FLAT WITH A GREAT VIEW", "A Luxury Studio Suite in Clerkenwell"), 69351, replace = TRUE),
  host_id = sample(1:1000000, 69351, replace = TRUE),
  host_name = sample(c("Susie", "Alina", "Luca", "Simon"), 69351, replace = TRUE),
  neighbourhood_group = sample(c(NA, NA, NA), 69351, replace = TRUE),
  neighbourhood = sample(c("Tower Hamlets", "Islington", "Camden", "Westminster"), 69351, replace = TRUE),
  latitude = runif(69351, min = 51.3, max = 51.7),
  longitude = runif(69351, min = -0.3, max = 0.1),
  room_type = sample(c("Private room", "Entire home/apt", "Shared room"), 69351, replace = TRUE),
  price = round(runif(69351, min = 30, max = 500)),
  minimum_nights = sample(1:30, 69351, replace = TRUE),
  number_of_reviews = sample(0:600, 69351, replace = TRUE),
  last_review = sample(seq(as.Date('2010-01-01'), as.Date('2023-01-01'), by="day"), 69351, replace = TRUE),
  reviews_per_month = round(runif(69351, min = 0, max = 5), 2),
  calculated_host_listings_count = sample(1:10, 69351, replace = TRUE),
  availability_365 = sample(0:365, 69351, replace = TRUE),
  number_of_reviews_ltm = sample(0:100, 69351, replace = TRUE),
  license = sample(c(NA, NA, NA), 69351, replace = TRUE)
)

# Display the structure of the simulated dataframe
#View(listings)

# Function to return the number of listings within a given price range
num_listings <- function(range){
  # Filter listings to be within the price range
  filter(listings, price >= range[1], price <= range[2]) %>% nrow()
}

# Function to create either boxplots or violin plots based on user selection and filtered data within a price range
make_plots <- function(range, choice){
  filtered_listings <- filter(listings, price >= range[1], price <= range[2])
  # Set the correct if-else conditions for boxplots or violin plots
  if (choice == "Box plots"){
    filtered_listings %>%
      ggplot(aes(y = price, x = room_type)) + 
      geom_boxplot() + 
      theme_classic()
  } else if (choice == "Violin plots"){
    filtered_listings %>%
      ggplot(aes(y = price, x = room_type)) + 
      geom_violin() + 
      theme_classic()
  }
}

num_private_rooms <- function(range){
  filter(listings, price >= range[1], price <= range[2]) %>%
    group_by(room_type) %>%
    summarise(prop = n()/nrow(listings)*100) %>%
    filter(room_type == "Private room") %>%
    pull(prop) %>% round(2)
}

median_price <- function(range){
  filter(listings, price >= range[1], price <= range[2]) %>%
    pull(price) %>% 
    median() %>% 
    round(2)
}



library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)
library(DT)

# Assuming the functions num_listings, num_private_rooms, median_price, make_plots, and the dataset listings are already defined

# UI
header <- dashboardHeader(title = "London Listings Dashboard")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Charts", tabName = "charts", icon = icon("chart-bar")),
    menuItem("Map", tabName = "map", icon = icon("map"))
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "charts",
            fluidRow(
              valueBoxOutput(outputId = "count"), 
              valueBoxOutput(outputId = "prop"),
              valueBoxOutput(outputId = "med")
            ),
            fluidRow(
              tabBox(side = "left", id = "tabset", height = "500px",
                     tabPanel("Charts", 
                              # Place plotly object here
                              fluidRow(box(plotlyOutput("plots", height = 500, width = 600)) ) ), 
                     # Place dataTable object here
                     tabPanel("Data table", height = "500px", dataTableOutput("table")) 
              ), 
              box(side = "right", height = "200px", title = "Welcome to London!"),
              box(side = "right", height = "385px", title = "Controls",
                  sliderInput(inputId = "range", label = "Select price range:",
                              min = 0, max = 25000, value = c(0, 2500)),
                  selectInput(inputId = "select", label = "Select group:", 
                              choices = c("Box plots", "Violin plots")) 
              ) 
            ) 
    ),
    tabItem(tabName = "map",
            # Place leaflet object here
            fluidRow(box(title = "Map of listings in London", leafletOutput("map", height = 600, width = 700))) 
    ) 
  ) 
)

# Set the correct arguments for dashboardPage()
ui <- dashboardPage(header, sidebar, body)

# Server
server <- function(input, output) {
  output$count <- renderValueBox({
    valueBox("Number of listings", num_listings(input$range), 
             icon = icon("house-user"))
  })
  
  output$prop <- renderValueBox({
    valueBox("Private rooms", paste0(num_private_rooms(input$range), "% of all listings"),
             icon = icon("eye"), color = "orange")
  })
  
  output$med <- renderValueBox({
    valueBox("Median price", paste0(median_price(input$range), "£"), 
             icon = icon("money-bill-alt"), color = "olive")
  })
  
  output$plots <- renderPlotly({
    make_plots(input$range, input$select)
  })
  
  output$table <- renderDataTable({
    filter(listings, price >= input$range[1], price <= input$range[2]) %>%
      select(name, neighbourhood, room_type, price)
  }, options = list(lengthMenu = c(5, 30, 50)))
  
  output$map <- renderLeaflet({
    leaflet(data = listings) %>%
      addTiles() %>%
      addCircleMarkers(~longitude, ~latitude, popup = ~name, radius = 1)
  })
}

# Run the application
shinyApp(ui, server)
