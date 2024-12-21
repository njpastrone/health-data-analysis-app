
# Load required libraries
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(reshape2)
library(ggcorrplot)
library(leaflet)
library(car)
library(lmtest)
library(broom)
library(gridExtra)
library(sf)


# Load data
data <- read.csv("data/merged_full_dataset.csv", stringsAsFactors = TRUE)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


# server
server <- function(input, output, session) {

  # correlation matrix 
  output$corr_matrix <- renderPlot({
    req(input$corr_vars)
    selected_data <- data[, input$corr_vars, drop = FALSE]
    corr_mat <- cor(selected_data, use = "complete.obs")
    ggcorrplot(corr_mat, hc.order = TRUE, type = "lower", lab = TRUE)
  })
  
  # EDA
  output$var_select_ui <- renderUI({
    lapply(1:input$num_vars, function(i) {
      selectInput(paste0("var", i), paste("Variable", i), 
                  choices = colnames(data), 
                  selected = colnames(data)[i])
    })
  })
  
  output$eda_plot <- renderPlot({
    req(input$generate_eda) 
    
    vars <- sapply(1:input$num_vars, function(i) input[[paste0("var", i)]])
    selected_data <- data[, vars, drop = FALSE]
    
    if (input$log_transform) {
      selected_data <- log1p(selected_data)
    }
    
    if (!all(sapply(selected_data, is.numeric))) {
      stop("All selected variables must be numeric.")
    }
    
    pairs(selected_data, main = "Pair Plot of Selected Variables")
  })

  # statistical analysis
  # Reactive filtered data
  filtered_data <- reactive({
    filter_and_impute_data(data, input$year_filter, input$country_filter, 
                           input$impute_data)
  })
  
  # Event for performing tests
  analysis_results <- eventReactive(input$run_analysis, {
    req(input$x_vars, input$y_vars, input$tests)
    perform_tests(filtered_data(), input$x_vars, input$y_vars, input$tests, 
                  input$log_transform_x, input$log_transform_y)
  })
  
  # Display test results
  output$test_results <- renderPrint({
    req(analysis_results())
    lapply(analysis_results(), function(res) {
      list(
        summary = summary(res$model),
        f_test = res$f_test,
        vif = res$vif,
        durbin_watson = res$durbin_watson,
        breusch_pagan = res$breusch_pagan,
        shapiro_wilk = res$shapiro_wilk
      )
    })
  })
  
  # Render residual plots
  output$residual_plot <- renderPlot({
    req(analysis_results(), input$show_plot)
    grid.arrange(grobs = generate_residual_plots(analysis_results()), ncol = 1)
  })

  # mapping
  # Reactive Filtering of Data
  filtered_data <- reactive({
    req(input$year, input$variable)
    
    data %>%
      filter(year == input$year) %>%
      select(Country.Name, Value = input$variable) %>%
      mutate(Value = as.numeric(Value)) %>%
      { 
        if (any(is.na(.$Value))) showNotification("Warning: Missing data detected!", 
                                                  type = "warning")
        .
      }
  })
  
  # Generate Choropleth Map
  output$choropleth_map <- renderLeaflet({
    req(filtered_data())
    map_data <- world %>%
      left_join(filtered_data(), by = c("name" = "Country.Name"))
    palette <- colorBin("YlGnBu", domain = map_data$Value, bins = 5, na.color = "gray")
    
    leaflet(map_data) %>%
      setView(lng = 0, lat = 20, zoom = 1) %>%  
      addTiles() %>%
      addPolygons(
        fillColor = ~palette(Value),
        weight = 1,
        color = "black",
        fillOpacity = 0.7,
        label = ~paste0(name, ": ", round(Value, 2)),
        highlightOptions = highlightOptions(
          weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
        )
      ) %>%
      addLegend(pal = palette, values = map_data$Value, title = input$variable, 
                position = "bottomright")
  })
  
  # Render Data Table
  output$data_table <- renderDT({ req(filtered_data()); filtered_data() })
  
  # data summary
  output$summary_table <- renderDataTable({
    req(input$generate_summary)
    vars <- input$summary_vars
    selected_data <- data[, vars, drop = FALSE]
    summary_list <- lapply(selected_data, function(column) {
      if (is.numeric(column)) {
        list(
          Type = "Numeric",
          Min = min(column, na.rm = TRUE),
          `1st Qu.` = quantile(column, 0.25, na.rm = TRUE),
          Median = median(column, na.rm = TRUE),
          Mean = mean(column, na.rm = TRUE),
          `3rd Qu.` = quantile(column, 0.75, na.rm = TRUE),
          Max = max(column, na.rm = TRUE)
        )
      } else {
        list(
          Type = "Non-numeric",
          Unique = length(unique(column)),
          Mode = names(sort(table(column), decreasing = TRUE))[1]
        )
      }
    })
    all_cols <- unique(unlist(lapply(summary_list, names)))
    summary_list <- lapply(summary_list, function(x) {
      x[setdiff(all_cols, names(x))] <- NA
      return(x)
    })
    summary_df <- do.call(rbind, lapply(names(summary_list), function(var_name) {
      data.frame(Variable = var_name, t(unlist(summary_list[[var_name]])), 
                 check.names = FALSE)
    }))
    datatable(summary_df, options = list(pageLength = 10))
  })  
  
}





# Run the App
shinyApp(ui, server)

