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
library(classInt) # For Jenks breaks
library(shinythemes)

# Load data
data <- read.csv("data/merged_full_dataset.csv", stringsAsFactors = TRUE)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Ensure the column `country_recoded` is available
if (!"country_recoded" %in% colnames(data)) {
  stop("The dataset must contain a column named 'country_recoded'.")
}

variable_choices <- setdiff(colnames(data), c("country", "country_recoded", "country_code"))
variable_choices_2 <- setdiff(colnames(data), c("country", "country_recoded", "country_code","year"))

# Helper Functions

# Variable Lists
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

corr_matrix_selection <- c("life_expectancy_at_birth", "gdp_per_capita_ppp",
                           "private_health_exppc_ppp", "government_health_exppc_ppp",
                           "unemployment_rate_national_estimate","urban_population_rate",
                           "kilocalorie_supply_per_capita")

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
  if (log_x) data <- data %>% mutate(across(all_of(x_vars), ~ log(. + 1)))
  if (log_y) data <- data %>% mutate(across(all_of(y_vars), ~ log(. + 1)))
  
  # Perform tests
  results <- lapply(y_vars, function(y) {
    panel_model <- plm(as.formula(paste(y, "~", paste(x_vars, collapse = " + "))), index = c("country_recoded", "year"), data = data)
    model <- lm(as.formula(paste(y, "~", paste(x_vars, collapse = " + "))), data = data)
    list(
      panel_model = summary(panel_model),
      model = model,
      vif = if ("VIF" %in% tests && length(x_vars) > 1) vif(model) else NULL,
      breusch_pagan = if ("Breusch-Pagan" %in% tests) bptest(panel_model) else NULL,
      shapiro_wilk = if ("Shapiro-Wilk" %in% tests && length(panel_model$residuals) <= 5000) 
        shapiro.test(panel_model$residuals) else NULL
    )
  })
  names(results) <- y_vars
  return(results)  # Corrected line
}
# cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, 
# readable, sandstone, simplex, slate, spacelab, superhero, united, yeti.
# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(tags$b("How much do you know about life expectancy?")),
  
  tabsetPanel(
    # 1. Introduction Tab
    tabPanel("Introduction",
             fluidPage(
               h2("Welcome!"),
               p("This app allows you to explore datasets interactively, offering tools for correlation analysis, exploratory data visualization, statistical tests, and mapping data on a world map."),
               
               h3("App Features"),
               tags$ul(
                 tags$li(strong("Correlation Matrix:"), " Analyze correlations between selected variables."),
                 tags$li(strong("Exploratory Data Analysis (EDA):"), " Visualize data with histograms, scatterplots, and pair plots."),
                 tags$li(strong("Statistical Analysis:"), " Perform statistical tests including Panel Linear Models, VIF, Breusch-Pagan, and Shapiro-Wilk tests."),
                 tags$li(strong("Choropleth Map:"), " Visualize data on a world map by year and selected variable."),
                 tags$li(strong("Data Summary and Raw Data:"), " Summarize and explore the dataset in tabular format.")
               ),
               
               h3("Key Variables"),
               p("Below are some key variables available in the dataset:"),
               tags$ul(
                 tags$li(strong("country:"), " The name of the country."),
                 tags$li(strong("year:"), " The year of observation."),
                 tags$li(strong("private_health_exppc_ppp:"), " Domestic private health expenditure per capita (in PPP)."),
                 tags$li(strong("government_health_exppc_ppp:"), " Government health expenditure per capita (in PPP)."),
                 tags$li(strong("gdp_per_capita_ppp:"), " GDP per capita (in PPP)."),
                 tags$li(strong("urban_population_rate:"), " Percentage of the population living in urban areas."),
                 tags$li(strong("life_expectancy_at_birth:"), " Life expectancy at birth (in years)."),
                 tags$li(strong("unemployment_rate_national_estimate:"), " National unemployment rate (in %)."),
                 tags$li(strong("kilocalorie_supply_per_capita:"), " Daily kilocalorie supply per capita.")
               ),
               
               h3("Metadata"),
               p("For detailed information on all variables in the dataset, download the metadata file:"),
               downloadLink("download_metadata", "Download Metadata"),
               h3("Contributors"),
               p("Note: This project was created for Dr. Richard Ressler's DATA-613 Data Science class at American University. All students contributed equally, and are listed here in alphabetical order:"),
               p("Hamed Ahmadi"),
               p("Nicolo Pastrone"),
               p("Suneela Maddineni"),
               p("Yanziwei Chen")
             )),
    
    # 2. Correlation Matrix Tab
    tabPanel("Correlation Matrix",
             sidebarLayout(
               sidebarPanel(
                 h3("Correlation Options"),
                 checkboxGroupInput("corr_vars", "Select Variables for Correlation:",
                                    choices = variable_choices_2, selected = corr_matrix_selection),
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
            choices = variable_choices, 
            selected = NULL,
            multiple = TRUE
          ),
          selectInput(
            "dependent_var",
            "Select Dependent Variable:",
            choices = variable_choices,
            selected = "life_expectancy_at_birth",
            multiple = FALSE
          ),
          h6("Note: Please select at least one dependent variable. Selecting two total variables will generate univariate histograms and a bivariate scatter plot. Selecting three or more total variables will generate pair plots."),
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
                 h4("Select Variables and Tests to Run"),
                 selectInput("x_vars", "Independent Variables:", choices = variable_choices, multiple = TRUE, selected = c("private_health_exppc_ppp", "government_health_exppc_ppp", "gdp_per_capita_ppp", "urban_population_rate", "unemployment_rate_national_estimate", "kilocalorie_supply_per_capita")),
                 selectInput("y_vars", "Dependent Variable:", choices = variable_choices, multiple = FALSE, selected = "life_expectancy_at_birth"),
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
                 fluidRow(
                   column(6, plotOutput("residual_histogram", width = "100%", height = "60vh")), # First plot in half the row
                   column(6, plotOutput("residual_plot", width = "100%", height = "60vh"))      # Second plot in the other half
                 )
               ))),
    
    # 5. Choropleth Map Tab
    # Tab for Choropleth Map Viewer
    tabPanel(
      title = "Choropleth Map",
      
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
    ),
    
    # 6. Data Summary Tab
    tabPanel("Data Summary",
             sidebarLayout(
               sidebarPanel(
                 h3("Data Summary Options"),
                 checkboxGroupInput("summary_vars", "Select Variables to Summarize:",
                                    choices = setdiff(colnames(data), c("country", "country_recoded", "country_code")),
                                    selected = corr_matrix_selection)
               ),
               mainPanel(
                 dataTableOutput("summary_table")
               )
             )
    ),
    
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
  
  output$download_metadata <- downloadHandler(
    filename = function() { "metadata.csv" },
    content = function(file) {
      file.copy("data/metadata.csv", file)
    }
  )
  
  # Correlation Matrix
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
    
    # Generate the correlation matrix plot with Flatly theme colors
    ggcorrplot(
      corr_mat,
      hc.order = FALSE,
      type = "lower",
      lab = TRUE,
      colors = c("#E74C3C", "#FFFFFF", "#2980B9"),  # Flatly colors: Red, White, Dark Blue
      outline.color = "white"
    ) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(color = "#2C3E50",angle=45,hjust=1), #The horizontal variable is tilted 45 degrees
        axis.text.y = element_text(color = "#2C3E50") # Flatly's dark gray text color
      )
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
  filtered_data_stat <- reactive({
    data
  })
  
  analysis_results <- eventReactive(input$run_analysis, {
    req(input$x_vars, input$y_vars, input$tests)
    perform_tests(filtered_data_stat(), input$x_vars, input$y_vars, input$tests, 
                  input$log_transform_x, input$log_transform_y)
  })
  
  
  output$test_results <- renderPrint({
    req(analysis_results())
    lapply(analysis_results(), function(res) {
      output_list <- list()
      if ("Panel Linear Model" %in% input$tests && !is.null(res$panel_model)) {
        output_list[["panel_model"]] <- res$panel_model
      }
      if ("VIF" %in% input$tests && !is.null(res$vif)) {
        output_list[["VIF"]] <- res$vif
      }
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
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
        theme_minimal()
    })
    grid.arrange(grobs = residual_plots, ncol = 1)
  })
  
  output$residual_histogram <- renderPlot({
    req(analysis_results(), input$show_histogram)
    
    residual_histograms <- lapply(analysis_results(), function(res) {
      ggplot(data.frame(Residuals = res$model$residuals), aes(x = Residuals)) +
        geom_histogram(bins = 30, fill = "blue", alpha = 0.7, color = "black") + # Specify bins
        labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") + # Correct labels
        theme_minimal()
    })
    
    grid.arrange(grobs = residual_histograms, ncol = 1)
  })
  
  # Choropleth Map Logic
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
    req(filtered_data_map())
    map_data <- world %>%
      left_join(filtered_data_map(), by = c("name" = "country_recoded"))
    
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
  
  # Render Data Table
  output$data_table <- renderDT({
    req(filtered_data_map())
    filtered_data_map() %>%
      mutate(Note = ifelse(is.na(Value), "Missing data", "Available"))
  })
  
  # Data Summary
  output$summary_table <- renderDataTable({
    req(input$summary_vars)
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
    
    # Ensure all rows have consistent columns
    all_cols <- c("Type", "Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "Unique", "Mode")
    summary_list <- lapply(summary_list, function(x) {
      x[setdiff(all_cols, names(x))] <- NA
      return(x[all_cols])
    })
    
    # Combine the summaries into a single data frame
    summary_df <- do.call(rbind, lapply(names(summary_list), function(var_name) {
      data.frame(Variable = var_name, t(unlist(summary_list[[var_name]])), check.names = FALSE)
    }))
    
    # Render the summary table
    datatable(summary_df, options = list(pageLength = 10))
  })
  
  
  
  # Raw Data Table
  output$data_table <- renderDT({
    data
  })
}

# Run the App
shinyApp(ui, server)
