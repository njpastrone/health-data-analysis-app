library(shiny)
library(car)
library(lmtest)
library(broom)
library(ggplot2)
library(dplyr)
library(gridExtra)

# Load sample data
data <- read.csv("data/merged_full_dataset.csv", stringsAsFactors = TRUE)

# Helper Function
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
    model <- lm(as.formula(paste(y, "~", paste(x_vars, collapse = " + "))), data = data)
    list(
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

# UI
ui <- fluidPage(
  titlePanel("Statistical Analysis App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_vars", "Independent Variables:", choices = colnames(data), multiple = TRUE),
      selectInput("y_vars", "Dependent Variable:", choices = colnames(data), multiple = FALSE),
      checkboxGroupInput("tests", "Select Tests:", choices = c("f_test", "vif", "durbin_watson", "breusch_pagan", "shapiro_wilk")),
      checkboxInput("log_transform_x", "Log Transform Independent Variables", value = FALSE),
      checkboxInput("log_transform_y", "Log Transform Dependent Variable", value = FALSE),
      actionButton("run_analysis", "Run Analysis"),
      checkboxInput("show_plot", "Show Residual Plots", value = FALSE)
    ),
    mainPanel(
      verbatimTextOutput("test_results"),
      plotOutput("residual_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filtered Data (mock implementation for now)
  filtered_data <- reactive({
    data # Replace with actual filtering logic if needed
  })
  
  # Perform Analysis
  analysis_results <- eventReactive(input$run_analysis, {
    req(input$x_vars, input$y_vars, input$tests)
    perform_tests(filtered_data(), input$x_vars, input$y_vars, input$tests, 
                  input$log_transform_x, input$log_transform_y)
  })
  
  # Display Test Results
  output$test_results <- renderPrint({
    req(analysis_results())
    lapply(analysis_results(), function(res) {
      # Create an empty list to store outputs for the selected tests
      output_list <- list()
      
      if ("f_test" %in% input$tests && !is.null(res$f_test)) {
        output_list[["F-Test"]] <- res$f_test
      }
      if ("vif" %in% input$tests && !is.null(res$vif)) {
        output_list[["VIF"]] <- res$vif
      }
      if ("durbin_watson" %in% input$tests && !is.null(res$durbin_watson)) {
        output_list[["Durbin-Watson Test"]] <- list(
          Statistic = res$durbin_watson$statistic,
          `P-Value` = res$durbin_watson$p.value
        )
      }
      if ("breusch_pagan" %in% input$tests && !is.null(res$breusch_pagan)) {
        output_list[["Breusch-Pagan Test"]] <- list(
          Statistic = res$breusch_pagan$statistic,
          `Degrees of Freedom` = res$breusch_pagan$parameter,
          `P-Value` = res$breusch_pagan$p.value
        )
      }
      if ("shapiro_wilk" %in% input$tests && !is.null(res$shapiro_wilk)) {
        output_list[["Shapiro-Wilk Test"]] <- list(
          Statistic = res$shapiro_wilk$statistic,
          `P-Value` = res$shapiro_wilk$p.value
        )
      }
      
      # Print only the selected test results
      for (test_name in names(output_list)) {
        cat(paste0("\n--- ", test_name, " ---\n"))
        print(output_list[[test_name]])
      }
    })
  })
  
  # Render Residual Plots
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
}

# Run the App
shinyApp(ui, server)
