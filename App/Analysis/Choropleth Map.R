# Load Required Libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(DT)

# Load Shapefile and Dataset
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
data <- read.csv("data/merged_full_dataset.csv", stringsAsFactors = FALSE)

# Ensure the column `country_recoded` is available
if (!"country_recoded" %in% colnames(data)) {
  stop("The dataset must contain a column named 'country_recoded'.")
}

# UI
ui <- fluidPage(
  titlePanel("Choropleth Map and Data Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      # Year Selection
      selectInput("year", "Select Year:", choices = unique(data$year), selected = unique(data$year)[1]),
      # Variable Selection
      selectInput("variable", "Select Variable for Map:", choices = colnames(data), selected = colnames(data)[2]),
      # Info Box
      p("Note: Missing data may appear for some years or metrics due to unrecorded metrics or lack of registration by countries.")
    ),
    
    mainPanel(
      # Choropleth Map Output
      leafletOutput("choropleth_map"),
      # Data Table Output
      DTOutput("data_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive Filtering of Data
  filtered_data <- reactive({
    req(input$year, input$variable)
    
    data %>%
      filter(year == input$year) %>%
      select(country_recoded, Value = input$variable) %>%
      mutate(Value = as.numeric(Value)) %>%
      { 
        # Show warning for missing data
        if (any(is.na(.$Value))) {
          showNotification(
            paste("Warning: Missing data detected for the selected year or metric.",
                  "This may be due to metrics not being recorded or countries not registering data."),
            type = "warning"
          )
        }
        .
      }
  })
  
  # Generate Choropleth Map
  output$choropleth_map <- renderLeaflet({
    req(filtered_data())
    map_data <- world %>%
      left_join(filtered_data(), by = c("name" = "country_recoded"))
    palette <- colorBin("YlGnBu", domain = map_data$Value, bins = 5, na.color = "gray")
    
    leaflet(map_data) %>%
      setView(lng = 0, lat = 20, zoom = 1) %>%  
      addTiles() %>%
      addPolygons(
        fillColor = ~palette(Value),
        weight = 1,
        color = "black",
        fillOpacity = 0.7,
        label = ~paste0(name, ": ", ifelse(is.na(Value), "No data", round(Value, 2))),
        highlightOptions = highlightOptions(
          weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
        )
      ) %>%
      addLegend(pal = palette, values = map_data$Value, title = input$variable, 
                position = "bottomright")
  })
  
  # Render Data Table
  output$data_table <- renderDT({
    req(filtered_data())
    filtered_data() %>%
      mutate(Note = ifelse(is.na(Value), "Missing data", "Available"))
  })
}

# Run the App
shinyApp(ui, server)
