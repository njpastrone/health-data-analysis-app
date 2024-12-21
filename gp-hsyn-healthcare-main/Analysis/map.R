library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(classInt) # For Jenks breaks

# Load data
data <- read.csv("data/merged_full_dataset.csv", stringsAsFactors = TRUE)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Ensure the column country_recoded is available
if (!"country_recoded" %in% colnames(data)) {
  stop("The dataset must contain a column named 'country_recoded'.")
}

variable_choices <- setdiff(colnames(data), c("country", "country_recoded", "country_code", "year"))
jenks_variables <- c("total_population", "male_population", "female_population", "gdp_ppp", "gni_ppp", "gdp_per_capita_ppp", "gni_per_capita_ppp")
negative_phenomenon_variables <- c(
  "diabetes_rate", "disease_mortality_rate", "infant_mortality_rate", "under_5_mortality_rate", 
  "tobacco_use_rate", "crude_deaths_per_thousand", "unemployment_rate_national_estimate", 
  "unemployment_rate_ilo_estimate", "obesity_rate_adults", "underweight_rate_children_under_5"
)

# Function to round large numbers to a reasonable approximation
round_large_numbers <- function(x) {
  ifelse(is.na(x), NA, round(x, -floor(log10(abs(x))) + 2))
}

# Define UI
ui <- fluidPage(
  titlePanel("Choropleth Map Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      # Year Selection
      selectInput("year", "Select Year:", choices = unique(data$year), selected = "2016"),
      # Variable Selection
      selectInput("variable", "Select Variable for Map:", choices = variable_choices, selected = "life_expectancy_at_birth"),
      # Color-Blind Friendly Option
      checkboxInput("colorBlind", "Color-Blind Friendly Palette", value = FALSE),
      # Info Box
      p("Note: Missing data may appear for some years or metrics due to unrecorded metrics or lack of registration by countries.")
    ),
    
    mainPanel(
      # Choropleth Map Output
      leafletOutput("choropleth_map")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive Filtering of Data
  filtered_data_map <- reactive({
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
    
    # Use appropriate binning strategy
    variable <- input$variable
    if (variable %in% jenks_variables) {
      breaks <- round_large_numbers(classInt::classIntervals(map_data$Value[!is.na(map_data$Value)], n = 5, style = "jenks")$brks)
      palette <- colorBin(
        palette = if (input$colorBlind) "viridis" else "YlGnBu", 
        domain = map_data$Value, 
        bins = breaks, 
        na.color = "gray"
      )
    } else if (variable %in% negative_phenomenon_variables) {
      breaks <- round(quantile(map_data$Value[!is.na(map_data$Value)], probs = seq(0, 1, length.out = 6), na.rm = TRUE), 1)
      palette <- colorBin(
        palette = if (input$colorBlind) "viridis" else "Reds", 
        domain = map_data$Value, 
        bins = breaks, 
        na.color = "gray"
      )
    } else if (!all(is.na(map_data$Value)) && length(unique(map_data$Value)) > 1) {
      breaks <- round(quantile(map_data$Value[!is.na(map_data$Value)], probs = seq(0, 1, length.out = 6), na.rm = TRUE), 1)
      palette <- colorBin(
        palette = if (input$colorBlind) "viridis" else "YlGnBu", 
        domain = map_data$Value, 
        bins = breaks, 
        na.color = "gray"
      )
    } else {
      palette <- colorBin(
        palette = if (input$colorBlind) "viridis" else "YlGnBu", 
        domain = map_data$Value, 
        bins = 5, 
        na.color = "gray"
      )
    }
    
    leaflet(map_data) %>%
      setView(lng = 0, lat = 20, zoom = 1) %>%  
      addTiles() %>%
      addPolygons(
        fillColor = ~palette(Value),
        weight = 1,
        color = "black",
        fillOpacity = 0.7,
        label = ~paste0(name, ": ", ifelse(is.na(Value), "No data", round(Value, 1))),
        highlightOptions = highlightOptions(
          weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
        )
      ) %>%
      addLegend(pal = palette, values = map_data$Value, title = input$variable, 
                position = "bottomright")
  })
}

# Run the App
shinyApp(ui, server)
