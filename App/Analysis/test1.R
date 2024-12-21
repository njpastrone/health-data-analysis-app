library(shiny)
library(leaflet)
library(DT)
library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(car)
library(lmtest)
library(broom)
library(gridExtra)
library(sf)
library(plm)

# Load data
data <- read.csv("data/merged_full_dataset.csv", stringsAsFactors = TRUE)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Helper Functions

corr_matrix_selection <- c("life_expectancy_at_birth", "gdp_per_capita_ppp", "adult_literacy_rate", "tobacco_use_rate")

filter_and_impute_data <- function(data, year_filter, country_filter, impute_data) {
  data %>%
    filter((is.null(year_filter) | year %in% year_filter),
           (is.null(country_filter) | Country.Name %in% country_filter)) %>%
    mutate(across(where(is.numeric), ~ ifelse(impute_data & is.na(.), mean(., na.rm = TRUE), .)))
}

perform_tests <- function(data, x_vars, y_vars, tests, log_x = FALSE, log_y = FALSE) {
  # Validate inputs
  if (length(x_vars) == 0 || length(y_vars) == 0) {
    stop("Please select at least one independent variable and one dependent variable.")
  }
  if (length(tests) == 0) {
    stop("Please select at least one statistical test.")
  }
  if (nrow(data) == 0) {
    stop("No data available for analysis. Check filters.")
  }
  
  # Log transformation
  if (log_x) data <- data %>% mutate(across(all_of(x_vars), ~ log10(. + 1)))
  if (log_y) data <- data %>% mutate(across(all_of(y_vars), ~ log10(. + 1)))
  
  # Perform tests
  results <- lapply(y_vars, function(y) {
    panel_model <- plm(as.formula(paste(y, "~", paste(x_vars, collapse = " + "))), index = c("country_recoded", "year"), data = data)
    model <- lm(as.formula(paste(y, "~", paste(x_vars, collapse = " + "))), data = data)
    list(
      panel_model = summary(panel_model),
      model = model,
      f_test = if ("f_test" %in% tests) tidy(anova(model)) else NULL,
      vif = if ("vif" %in% tests && length(x_vars) > 1) vif(model) else NULL,
      durbin_watson = if ("durbin_watson" %in% tests) dwtest(model) else NULL,
      breusch_pagan = if ("breusch_pagan" %in% tests) bptest(model) else NULL,
      shapiro_wilk = if ("shapiro_wilk" %in% tests && length(model$residuals) <= 5000) 
        shapiro.test(model$residuals) else NULL
    )
  })
  names(results) <- y_vars
  results
}

# Define UI
ui <- fluidPage(
  titlePanel("Life Expectancy and Statistical Analysis App"),
  
  tabsetPanel(
    # 1. Introduction Tab
    tabPanel("Introduction",
             fluidPage(
               h3("Welcome to the Statistical Analysis App"),
               p("This app allows you to explore datasets interactively. It includes features like correlation analysis, 
                 exploratory data visualization, statistical tests, and mapping data on a world map."),
               h3("Features"),
               p("Use the tabs to navigate between functionalities. Each tab offers specialized tools for exploring and analyzing data.")
             )),
    
    # 2. Correlation Matrix Tab
    tabPanel("Correlation Matrix",
             sidebarLayout(
               sidebarPanel(
                 h3("Correlation Options"),
                 checkboxGroupInput("corr_vars", "Select Variables for Correlation:",
                                    choices = colnames(data), selected = corr_matrix_selection),
                 actionButton("generate_corr", "Generate Correlation Matrix")
               ),
               mainPanel(
                 plotOutput("corr_matrix",  width = "90%", height = "72vh")
               )
             )),
    
    # 3. Exploratory Data Analysis (EDA) Tab
    tabPanel(
      "Exploratory Data Analysis",
      sidebarLayout(
        sidebarPanel(
          h4("EDA Options"),
          selectInput(
            "independent_vars",
            "Select Independent Variable(s):",
            choices = colnames(data), 
            selected = NULL,
            multiple = TRUE
          ),
          selectInput(
            "dependent_var",
            "Select Dependent Variable:",
            choices = colnames(data),
            selected = "life_expectancy_at_birth",
            multiple = FALSE
          ),
          actionButton("generate_plot", "Generate Plot") 
        ),
        mainPanel(
          plotOutput("eda_plot",  width = "100%", height = "80vh")
        )
      )
    ),
    
    # 4. Statistical Analysis Tab (Integrated)
    tabPanel("Statistical Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("x_vars", "Independent Variables:", choices = colnames(data), multiple = TRUE),
                 selectInput("y_vars", "Dependent Variable:", choices = colnames(data), multiple = FALSE),
                 checkboxGroupInput("tests", "Select Tests:", choices = c("panel linear model","f_test", "vif", "durbin_watson", "breusch_pagan", "shapiro_wilk")),
                 checkboxInput("log_transform_x", "Log Transform Independent Variables", value = FALSE),
                 checkboxInput("log_transform_y", "Log Transform Dependent Variable", value = FALSE),
                 actionButton("run_analysis", "Run Analysis"),
                 checkboxInput("show_plot", "Show Residual Plots", value = FALSE)
               ),
               mainPanel(
                 verbatimTextOutput("test_results"),
                 plotOutput("residual_plot")
               )
             )),
    
    # 5. Choropleth Map Tab
    tabPanel("Choropleth Map",
             sidebarLayout(
               sidebarPanel(
                 selectInput("year", "Year:", choices = unique(data$year)),
                 selectInput("variable", "Variable:", choices = colnames(data), selected = "country_recoded"),
                 actionButton("generate_map", "Generate Map")
               ),
               mainPanel(
                 leafletOutput("choropleth_map"),
                 DTOutput("data_table")
               )
             )),
    
    # 6. Data Summary Tab
    tabPanel("Data Summary",
             sidebarLayout(
               sidebarPanel(
                 h3("Data Summary Options"),
                 checkboxGroupInput("summary_vars", "Select Variables to Summarize:",
                                    choices = colnames(data), selected = colnames(data)),
                 actionButton("generate_summary", "Generate Summary")
               ),
               mainPanel(
                 dataTableOutput("summary_table")
               )
             )),
    
    # 7. Raw Data Tab
    tabPanel("Raw Data",
             fluidPage(
               h3("View Raw Data"),
               DTOutput("data_table")
             ))
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Correlation Matrix
  output$corr_matrix <- renderPlot({
    req(input$corr_vars)
    selected_data <- data[, input$corr_vars, drop = FALSE]
    
    # Ensure all selected variables are numeric
    if (!all(sapply(selected_data, is.numeric))) {
      plot.new()
      text(0.5, 0.5, "Selected variables must be numeric.")
      return()
    }
    
    # Ensure at least two variables are selected
    if (ncol(selected_data) < 2) {
      plot.new()
      text(0.5, 0.5, "Please select at least two variables.")
      return()
    }
    
    # Compute correlation matrix
    corr_mat <- cor(selected_data, use = "complete.obs")
    
    # Generate correlation matrix plot
    ggcorrplot(corr_mat, hc.order = TRUE, type = "lower", lab = TRUE, colors = c("blue", "white", "red"))
  })
  
  # EDA
  output$eda_plot <- renderPlot({
    req(input$generate_plot) 
    req(input$independent_vars) 
    req(input$dependent_var) 
    
    independent_vars <- input$independent_vars
    dependent_var <- input$dependent_var
    
    selected_data <- data[, c(independent_vars, dependent_var), drop = FALSE]
    
    if (!all(sapply(selected_data, is.numeric))) {
      stop("All selected variables must be numeric.")
    }
    
    if (length(independent_vars) == 1) {
      independent_var <- independent_vars[1] 
      par(mfrow = c(2, 2)) 
      hist(selected_data[[dependent_var]], main = paste("Histogram of", dependent_var), xlab = dependent_var, col = "lightpink", border = "white")
      hist(selected_data[[independent_var]], main = paste("Histogram of", independent_var), xlab = independent_var, col = "lightblue", border = "white")
      plot(selected_data[[independent_var]], selected_data[[dependent_var]], main = paste("Scatterplot of", independent_var, "vs", dependent_var),
           xlab = independent_var, ylab = dependent_var, pch = 19, col = "darkblue")
      par(mfrow = c(1, 1)) 
    } else if (length(independent_vars) > 1) {
      pairs(selected_data, main = "Pairs Plot of Selected Variables", pch = 21, bg = "lightblue")
    }
  })
  
  # Statistical Analysis Logic
  filtered_data <- reactive({
    data
  })
  
  analysis_results <- eventReactive(input$run_analysis, {
    req(input$x_vars, input$y_vars, input$tests)
    perform_tests(filtered_data(), input$x_vars, input$y_vars, input$tests, 
                  input$log_transform_x, input$log_transform_y)
  })
  
  output$test_results <- renderPrint({
    req(analysis_results())
    lapply(analysis_results(), function(res) {
      output_list <- list()
      if ("panel linear model" %in% input$tests && !is.null(res$panel_)) {
        output_list[["panel_model"]] <- res$panel_model
      }
      if ("f_test" %in% input$tests && !is.null(res$f_test)) {
        output_list[["F-Test"]] <- res$f_test
      }
      if ("vif" %in% input$tests && !is.null(res$vif)) {
        output_list[["VIF"]] <- res$vif
      }
      if ("durbin_watson" %in% input$tests && !is.null(res$durbin_watson)) {
        output_list[["Durbin-Watson Test"]] <- list(Statistic = res$durbin_watson$statistic, `P-Value` = res$durbin_watson$p.value)
      }
      if ("breusch_pagan" %in% input$tests && !is.null(res$breusch_pagan)) {
        output_list[["Breusch-Pagan Test"]] <- list(Statistic = res$breusch_pagan$statistic, `Degrees of Freedom` = res$breusch_pagan$parameter, `P-Value` = res$breusch_pagan$p.value)
      }
      if ("shapiro_wilk" %in% input$tests && !is.null(res$shapiro_wilk)) {
        output_list[["Shapiro-Wilk Test"]] <- list(Statistic = res$shapiro_wilk$statistic, `P-Value` = res$shapiro_wilk$p.value)
      }
      
      for (test_name in names(output_list)) {
        cat(paste0("\n--- ", test_name, " ---\n"))
        print(output_list[[test_name]])
      }
    })
  })
  
  output$residual_plot <- renderPlot({
    req(analysis_results(), input$show_plot)
    residual_plots <- lapply(analysis_results(), function(res) {
      ggplot(data.frame(Fitted = res$model$fitted.values, Residuals = res$model$residuals),
             aes(x = Fitted, y = Residuals)) +
        geom_point() +
        geom_smooth(method = "loess", se = FALSE, color = "blue") +
        labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
        theme_minimal()
    })
    grid.arrange(grobs = residual_plots, ncol = 1)
  })
  
  # Choropleth Map Logic
  filtered_map_data <- reactive({
    req(input$year, input$variable)
    data %>%
      filter(year == input$year) %>%
      select(Country.Name, Value = input$variable) %>%
      mutate(Value = as.numeric(Value))
  })
  
  output$choropleth_map <- renderLeaflet({
    req(filtered_map_data())
    map_data <- world %>%
      left_join(filtered_map_data(), by = c("name" = "Country.Name"))
    palette <- colorBin("YlGnBu", domain = map_data$Value, bins = 5, na.color = "gray")
    
    leaflet(map_data) %>%
      setView(lng = 0, lat = 20, zoom = 1) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~palette(Value),
        weight = 1,
        color = "black",
        fillOpacity = 0.7,
        label = ~paste0(name, ": ", round(Value, 2))
      ) %>%
      addLegend(pal = palette, values = map_data$Value, title = input$variable)
  })
  
  output$data_table <- renderDT({
    req(filtered_map_data())
    filtered_map_data()
  })
  
  # Data Summary
  output$summary_table <- renderDataTable({
    req(input$generate_summary)
    selected_data <- data[, input$summary_vars, drop = FALSE]
    summary(selected_data)
  })  
  
  # Raw Data Table
  output$data_table <- renderDT({
    data
  })
}

# Run the App
shinyApp(ui, server)
