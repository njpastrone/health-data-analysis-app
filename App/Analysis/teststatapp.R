library(shiny)
library(leaflet)

data <- read.csv("data/merged_full_dataset.csv", stringsAsFactors = TRUE)

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
     # f_test = if ("f_test" %in% tests) tidy(anova(panel_model)) else NULL,
      vif = if ("VIF" %in% tests && length(x_vars) > 1) vif(model) else NULL,
    #  durbin_watson = if ("durbin_watson" %in% tests) dwtest(panel_model) else NULL,
      breusch_pagan = if ("Breusch-Pagan" %in% tests) bptest(panel_model) else NULL,
      shapiro_wilk = if ("Shapiro-Wilk" %in% tests && length(panel_model$residuals) <= 5000) 
        shapiro.test(panel_model$residuals) else NULL
    )
  })
  names(results) <- y_vars
  return(results)  # Corrected line
}

ui <- fluidPage(
# 4. Statistical Analysis Tab (Integrated)
tabPanel("Statistical Analysis",
         sidebarLayout(
           sidebarPanel(
             h4("Select Variables and Tests to Run"),
             selectInput("x_vars", "Independent Variables:", choices = colnames(data), multiple = TRUE, selected = c("private_health_exppc_ppp", "government_health_exppc_ppp", "gdp_per_capita_ppp", "urban_population_rate", "unemployment_rate_national_estimate", "kilocalorie_supply_per_capita")),
             selectInput("y_vars", "Dependent Variable:", choices = colnames(data), multiple = FALSE, selected = "life_expectancy_at_birth"),
             checkboxGroupInput("tests", "Select Tests:", choices = c("Panel Linear Model", "VIF", "Breusch-Pagan", "Shapiro-Wilk"), selected = "Panel Linear Model"),
             h6("Note: Shapiro-wilk only available when n < 5000. Normality of residuals assumption can be relaxed with large sample size."),             
             checkboxInput("log_transform_x", "Log Transform Independent Variables", value = FALSE),
             checkboxInput("log_transform_y", "Log Transform Dependent Variable", value = FALSE),
             checkboxInput("show_histogram", "Show Residual Histogram", value = FALSE),
             checkboxInput("show_plot", "Show Residual Scatter", value = FALSE),           
             actionButton("run_analysis", "Run Analysis"),
             h4("Test Descriptions"),
             h6("Commonly, a p-value  of less than 0.05 is regarded as 'statistically significant', and one can reject the test's null hypothesis in favor of the alternative hypothesis. Common levels of significance range from p-value < 0.01 to p-value < 0.1."),
             h5("Panel Linear Model:"),
             h6("Purpose: Runs a linear model for panel data - data with multiple observations over multiple time periods for the same entity. Controls for individual heterogeneity and autocorrelation across time."),
             h6("Interpretation: Each p-value corresponds to the individual significance of each parameter in the model."),
             h6("Hypotheses (for each beta): \n H0: beta = 0, \n HA: beta != 0"),
             h5("Variance Inflation Factor (VIF):"),
             h6("Purpose: The Variance Inflation Factor (VIF) measures the degree of multicollinearity among the independent variables in a regression model. High VIF values indicate that an independent variable is highly correlated with one or more other independent variables, which can lead to unreliable coefficient estimates."),
             h6("Interpretation: A VIF value greater than 10 is often considered indicative of significant multicollinearity, suggesting that the variable may need to be removed or combined with others to improve model stability."),
             h6("Hypotheses: \n H0: No multicollinearity exists among the independent variables. \n HA: Multicollinearity exists among the independent variables."),
             h5("Breusch-Pagan Test:"),
             h6("Purpose: The Breusch-Pagan test assesses whether the residuals from a regression model exhibit heteroscedasticity, meaning that their variance changes across levels of an independent variable. Detecting heteroscedasticity is crucial for ensuring valid statistical inferences."),
             h6("Interpretation: A low p-value indicates that heteroscedasticity is present in the residuals, suggesting that the assumption of constant variance may be violated. This may necessitate adjustments to the model, such as using robust standard errors."),
             h6("Hypotheses: \n H0: The residuals have constant variance (homoscedasticity). \n HA: The residuals do not have constant variance (heteroscedasticity)."),
             h5("Shapiro-Wilk Test:"),
             h6("Purpose: The Shapiro-Wilk test evaluates the normality of the residuals from a regression model. Normality of residuals is an important assumption in linear regression, affecting the validity of hypothesis tests and confidence intervals."),
             h6("Interpretation: A low p-value indicates that the residuals significantly deviate from a normal distribution, suggesting that the assumption of normality may be violated. This could lead to concerns about the reliability of statistical inferences made from the model."),
             h6("Hypotheses: \n H0: The residuals are normally distributed. \n HA: The residuals are not normally distributed.")
           ),
           mainPanel(
             verbatimTextOutput("test_results"),
             plotOutput("residual_histogram"),
             plotOutput("residual_plot")
           )
         ))
)




server <- function(input, output, session) {
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
    if ("Panel Linear Model" %in% input$tests && !is.null(res$panel_model)) {
      output_list[["panel_model"]] <- res$panel_model
    }
#    if ("f_test" %in% input$tests && !is.null(res$f_test)) {
 #     output_list[["F-Test"]] <- res$f_test
  #  }
    if ("VIF" %in% input$tests && !is.null(res$vif)) {
      output_list[["VIF"]] <- res$vif
    }
#    if ("durbin_watson" %in% input$tests && !is.null(res$durbin_watson)) {
 #     output_list[["Durbin-Watson Test"]] <- list(Statistic = res$durbin_watson$statistic, `P-Value` = res$durbin_watson$p.value)
  #  }
    if ("Breusch-Pagan" %in% input$tests && !is.null(res$breusch_pagan)) {
      output_list[["Breusch-Pagan Test"]] <- list(Statistic = res$breusch_pagan$statistic, `Degrees of Freedom` = res$breusch_pagan$parameter, `P-Value` = res$breusch_pagan$p.value)
    }
    if ("Shapiro-Wilk" %in% input$tests && !is.null(res$shapiro_wilk)) {
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

output$residual_histogram <- renderPlot({
  req(analysis_results(), input$show_histogram)
  
  # Create a list of histograms for each result
  residual_histograms <- lapply(analysis_results(), function(res) {
    ggplot(data.frame(Residuals = res$model$residuals), aes(x = Residuals)) +
      geom_histogram(bins = 30, fill = "blue", alpha = 0.7, color = "black") + # Specify bins
      labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") + # Correct labels
      theme_minimal()
  })
  
  # Arrange plots in a grid
  grid.arrange(grobs = residual_histograms, ncol = 1)
})
}

# Run the App
shinyApp(ui, server)


