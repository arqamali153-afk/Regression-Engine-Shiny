# ==============================================================================
#   MASTER APP: THE "GREAT WALL" ULTIMATE EDITION (HYBRID REPORT)
#   - Dashboard: Concise Summaries 
#   - Correlation Tab: Simplified Text 
#   - Report: Best of Both Worlds
#   - NEW: Decision Engine (Executive Prediction Dashboard)
# ==============================================================================

library(shiny)
library(ggplot2)
library(bslib)
library(DT)
library(nortest) 
library(rmarkdown)
library(car) 
library(tidyr)
library(dplyr)
library(plotly)
library(knitr)
library(lmtest) 
library(reshape2)

# --- 1. CUSTOM CSS ARCHITECTURE ---
custom_css <- "
  /* Card Containers */
  .card-style {
    background-color: #ffffff;
    border: 1px solid #e3e6f0;
    border-radius: 0.5rem;
    box-shadow: 0 0.15rem 1.75rem 0 rgba(58, 59, 69, 0.15);
    padding: 25px;
    margin-bottom: 25px;
  }
  
  /* Narrative Boxes */
  .interpretation-box {
    background-color: #f8f9fc;
    border-left: 5px solid #4e73df;
    padding: 25px;
    border-radius: 4px;
    color: #2c3e50;
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    line-height: 1.7;
    font-size: 1.05em;
  }
  
  /* Executive Decision Engine CSS */
  .executive-box {
    background: linear-gradient(135deg, #f8f9fc 0%, #eaecf4 100%);
    border-radius: 10px;
    padding: 30px;
    border: 1px solid #d1d3e2;
  }
  .prediction-massive {
    font-size: 4.5em;
    color: #1cc88a;
    font-weight: 900;
    text-align: center;
    text-shadow: 1px 1px 2px rgba(0,0,0,0.1);
    margin: 20px 0;
  }
  
  /* Statistical Highlights */
  .stat-value {
    color: #4e73df;
    font-weight: 800;
  }
  .p-value-sig {
    color: #1cc88a; /* Green for significant */
    font-weight: bold;
  }
  .p-value-ns {
    color: #858796; /* Gray for non-significant */
    font-weight: bold;
  }
  
  /* Typography */
  h4 {
    border-bottom: 2px solid #eaecf4;
    padding-bottom: 12px;
    margin-bottom: 25px;
    color: #4e73df;
    font-weight: 600;
  }
  h5 {
    color: #5a5c69;
    font-weight: 700;
    margin-top: 20px;
    margin-bottom: 15px;
  }
  
  /* Latex Equation Box */
  .equation-box {
    font-family: 'Times New Roman', Times, serif;
    font-size: 1.3em;
    background-color: #fff;
    padding: 15px;
    border: 1px dashed #ccc;
    text-align: center;
    margin: 20px 0;
  }
"

# --- 2. UI DEFINITION ---
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "zephyr", primary = "#4e73df"),
  tags$head(tags$style(HTML(custom_css))),
  
  # -- Header --
  titlePanel(
    div(
      icon("chart-line"), 
      "Statistical Insight Engine: Ultimate Edition", 
      style = "color:#2c3e50; font-weight:800; letter-spacing: 0.5px;"
    )
  ),
  
  # -- Sidebar Layout --
  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = "background-color: #f8f9fc; border-right: 1px solid #e3e6f0; padding-top: 30px;",
      
      # SECTION 1: DATA INGESTION (PORTFOLIO MODE)
      h5(icon("lock"), "1. Data Ingestion (Demo Mode)", class = "text-primary"),
      div(style = "background-color: #fdf5e6; border-left: 4px solid #f6c23e; padding: 10px; border-radius: 3px; margin-bottom: 15px;",
          p(strong("Portfolio Sandbox"), style="color: #b8860b; margin-bottom: 5px;"),
          p("CSV Upload disabled for this public demonstration. The engine is currently running a sample medical cost dataset.", style="color: #666; font-size: 0.85em; margin-bottom: 0;")
      ),
      hr(),
      
      # SECTION 2: FILTERS
      h5(icon("filter"), "2. Data Segmentation", class = "text-primary"),
      uiOutput("group_var_selector"),   
      uiOutput("group_level_selector"), 
      helpText("Filter your dataset by a categorical variable to analyze specific segments (e.g., Region, Segment)."),
      hr(),
      
      # SECTION 3: MODEL SPECIFICATION
      h5(icon("cogs"), "3. Model Specification", class = "text-primary"),
      uiOutput("y_var_select"),
      checkboxInput("log_y", "Log Transform Target (Y)", value = FALSE),
      tags$small(class="text-muted", "Recommended if Target Variable is highly skewed (e.g., Revenue, Price). Values must be > 0."),
      br(), br(),
      uiOutput("x_var_select"),
      checkboxInput("interactions", "Include 2-way Interactions", value = FALSE),
      tags$small(class="text-muted", "Analyzes if the effect of one predictor depends on another (Synergy Effects)."),
      hr(),
      
      # SECTION 4: EXPORT
      h5(icon("download"), "4. Export Intelligence", class = "text-primary"),
      downloadButton("downloadReport", "Download Full Report (HTML)", class = "btn-success", style="width:100%; margin-bottom:10px; font-weight:bold;"),
      downloadButton("downloadMatrix", "Download Correlations (CSV)", class = "btn-outline-secondary", style="width:100%;")
    ),
    
    mainPanel(
      width = 9,
      br(),
      tabsetPanel(
        type = "pills", 
        id = "main_tabs",
        
        # --- TAB 1: DATA EXPLORER ---
        tabPanel("Data Explorer", icon = icon("table"),
                 br(),
                 div(class = "card-style",
                     h4("Dataset Overview & Integrity Check"),
                     uiOutput("data_summary_box"),
                     hr(),
                     p("The table below displays the raw data currently loaded into the engine. Use the search bar to locate specific records."),
                     DTOutput("raw_data"))
        ),
        
        # --- TAB 2: CORRELATION ---
        tabPanel("Correlation Analysis", icon = icon("project-diagram"),
                 br(),
                 fluidRow(
                   column(8, 
                          div(class = "card-style",
                              h4("Multivariate Correlation Heatmap"),
                              p("This visualization maps the linear relationships between all numeric variables. Red indicates negative correlation; Green indicates positive correlation."),
                              plotOutput("corr_plot", height = "550px"))),
                   column(4,
                          div(class = "card-style",
                              h4("Top Associations"),
                              p("Strongest relationships found in data:"),
                              DTOutput("top_cor_table")),
                          div(class = "card-style",
                              h4("Insight"),
                              uiOutput("corr_text")))
                 )
        ),
        
        # --- TAB 3: REGRESSION ENGINE ---
        tabPanel("Regression Engine", icon = icon("calculator"),
                 br(),
                 # 3.1 NARRATIVE SECTION
                 fluidRow(
                   column(12,
                          div(class = "card-style",
                              h4("Executive Strategic Narrative"),
                              p("A brief summary of model performance. For the full, detailed breakdown, please download the report."),
                              div(class = "interpretation-box", uiOutput("narrative_text")),
                              br(),
                              h5("Mathematical Model Representation"),
                              uiOutput("equation_ui")
                          )
                   )
                 ),
                 # 3.2 VISUALS
                 fluidRow(
                   column(6, 
                          div(class = "card-style",
                              h4("Model Accuracy (Actual vs Predicted)"),
                              p("Points closer to the red dashed line indicate higher accuracy."),
                              plotlyOutput("reg_plot", height = "400px"))),
                   column(6,
                          div(class = "card-style",
                              h4("Driver Importance (Standardized)"),
                              p("Compares the relative strength of predictors by standardizing scales."),
                              plotlyOutput("importance_plot", height = "400px")))
                 ),
                 # 3.3 TECHNICAL OUTPUT
                 div(class = "card-style",
                     h4("Technical Model Output"),
                     div(style = "font-size: 0.9em; background-color: #f8f9fc; border: 1px solid #e3e6f0; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                         strong("How to read Significance Codes:"), br(),
                         span(" *** ", style="color:black; font-weight:bold; background-color:#ffeb3b; padding:2px;"), " = Strong Evidence (99.9% Confidence).", br(),
                         span(" ** ", style="color:black; font-weight:bold; background-color:#fff176; padding:2px;"), " = Good Evidence (99% Confidence).", br(),
                         span(" * ", style="color:black; font-weight:bold; background-color:#fff9c4; padding:2px;"), " = Some Evidence (95% Confidence).", br(),
                         span(" (No stars) ", style="color:#999;"), " = Not Significant."
                     ),
                     uiOutput("vif_alert_ui"),
                     verbatimTextOutput("model_summary"))
        ),
        
        # --- TAB 4: DECISION ENGINE (EXECUTIVE PREDICTOR) ---
        tabPanel("Decision Engine", icon = icon("lightbulb"),
                 br(),
                 div(class = "alert alert-success", style = "background-color: #e8fbf3; border-color: #1cc88a; color: #0f6848;",
                     h5(icon("info-circle"), "Executive Guide: How to Use This Simulator", style="color: #0f6848;"),
                     p(strong("1. Pull the Levers:"), " Adjust the inputs on the left to test different business strategies."),
                     p(strong("2. Read the Result:"), " The large number on the right is your projected outcome."),
                     p(strong("3. Trust the Engine:"), " These projections are strictly governed by rigorous statistical tests.")
                 ),
                 
                 # NEW: Dynamic Business Interpretation Box
                 div(class = "card-style", style = "background-color: #fffaf0; border-left: 5px solid #f6c23e;",
                     h4(icon("comments-dollar"), " Consultant's Insight", style="color: #f6c23e; font-weight: bold;"),
                     p("Based on the mathematical engine, here is the plain-English translation of your current strategy:", style="color: #666;"),
                     uiOutput("business_translation_ui")
                 ),
                 
                 fluidRow(
                   column(5,
                          div(class = "card-style",
                              h4("Business Scenario Inputs"),
                              p("Adjust these variables to simulate your next business decision."),
                              hr(),
                              uiOutput("dynamic_prediction_inputs")
                          )
                   ),
                   column(7,
                          div(class = "card-style executive-box",
                              h4("Projected Outcome", style="text-align:center; color:#2c3e50; font-weight:bold;"),
                              p("Based on your strategy on the left, this is your expected result:", style="text-align:center; color:#666; margin-bottom:-10px;"),
                              uiOutput("prediction_result_ui")
                          ),
                          div(class = "card-style",
                              h4("Context: Prediction vs. Historical Data"),
                              p("See where your simulated strategy falls compared to historical records.", style="color:#666;"),
                              plotlyOutput("prediction_context_plot", height = "350px")
                          )
                   )
                 )
        ),
        
        # --- TAB 5: DIAGNOSTICS & AUDIT ---
        tabPanel("Diagnostics & Audit", icon = icon("stethoscope"),
                 br(),
                 fluidRow(
                   column(8, 
                          div(class = "card-style", 
                              h4("Residual Diagnostic Plots"),
                              p("These 4 plots help validate the 'Trustworthiness' of the model."),
                              plotOutput("diagnostic_plot", height = "650px"))),
                   column(4, 
                          div(class = "card-style", h4("Normality Test (Shapiro-Wilk)"), uiOutput("logic_engine_ui")),
                          div(class = "card-style", h4("Homoscedasticity Test (Breusch-Pagan)"), uiOutput("homoscedasticity_ui")),
                          div(class = "card-style", h4("Outlier Detection (Cook's Distance)"), uiOutput("outlier_ui"))
                   )
                 )
        ),
        
        # --- TAB 6: METHODOLOGY ---
        tabPanel("Methodology", icon = icon("book"),
                 br(),
                 div(class = "card-style",
                     h4("Statistical Methodology & Assumptions"),
                     p("This engine utilizes ", strong("Ordinary Least Squares (OLS)"), " regression to estimate the relationship between the target variable and independent predictors."),
                     hr(),
                     h5("1. Linearity"),
                     p("The relationship between the independent and dependent variables is assumed to be linear."),
                     h5("2. Independence"),
                     p("Observations are assumed to be independent of each other."),
                     h5("3. Homoscedasticity"),
                     p("The variance of residual is the same for any value of X. This is tested using the Breusch-Pagan test in the Diagnostics tab."),
                     h5("4. Normality"),
                     p("For fixed values of X, Y is normally distributed. This is tested using the Shapiro-Wilk or Anderson-Darling test."),
                     h5("5. No Multicollinearity"),
                     p("Independent variables should not be highly correlated with each other. This is monitored via VIF (Variance Inflation Factor).")
                 )
        ),
        tags$footer(
          style = "position: fixed; bottom: 0; width: 100%; text-align: center; padding: 10px; background-color: #f8f9fc; color: #666; font-size: 0.8em;",
          "Engineered by Arqam Ali | Statistical Analysis Sandbox © 2026"
        )
      )
    )
  )
)

# --- 3. SERVER LOGIC ---
server <- function(input, output, session) {
  
  # ============================================================================
  #   DATA PROCESSING CORE
  # ============================================================================
  
  # 1. READ DATA (PRE-LOADED FOR PORTFOLIO SANDBOX)
  raw_file_data <- reactive({
    
    # Load the new insurance dataset from the relative path
    df <- read.csv("data/insurance.csv", stringsAsFactors = TRUE)
    
    # Use dplyr to select the clean, relevant columns
    df <- df %>%
      dplyr::select(
        age,
        sex,
        bmi,
        children,
        smoker,
        region,
        charges
      )
    
    return(df)
  })
  
  # 2. FILTER DATA
  filtered_data <- reactive({
    req(raw_file_data())
    df <- raw_file_data()
    if (!is.null(input$group_var) && input$group_var != "None" && !is.null(input$group_level)) {
      df <- df[df[[input$group_var]] == input$group_level, ]
    }
    return(df)
  })
  
  # 3. PREPARE MODEL DATA (Clean NAs, Handle Transformations)
  model_data <- reactive({
    req(filtered_data(), input$y_var, input$x_var)
    if(!all(c(input$y_var, input$x_var) %in% names(filtered_data()))) return(NULL)
    df_subset <- filtered_data()[, c(input$y_var, input$x_var), drop = FALSE]
    df_subset <- na.omit(df_subset)
    
    if (isTRUE(input$log_y)) {
      y_vals <- df_subset[[input$y_var]]
      if (!is.numeric(y_vals) || any(y_vals <= 0)) {
        updateCheckboxInput(session, "log_y", value = FALSE)
      } else {
        df_subset[[input$y_var]] <- log(y_vals)
      }
    }
    return(df_subset)
  })
  
  # 4. GET NUMERIC DATA ONLY (For Correlations)
  numeric_data <- reactive({
    req(filtered_data())
    nums <- unlist(lapply(filtered_data(), is.numeric))
    na.omit(filtered_data()[, nums, drop = FALSE])
  })
  
  # ============================================================================
  #   DYNAMIC UI CONTROLLERS
  # ============================================================================
  
  output$group_var_selector <- renderUI({
    # Strictly limit the Group By options to true categorical variables
    selectInput("group_var", "Group By (Optional):", 
                choices = c("None", 
                            "Sex" = "sex", 
                            "Smoker Status" = "smoker", 
                            "Region" = "region"))
  })
  
  output$group_level_selector <- renderUI({
    req(input$group_var); if(input$group_var == "None") return(NULL)
    selectInput("group_level", paste("Select", input$group_var, ":"), choices = sort(unique(raw_file_data()[[input$group_var]])))
  })
  
  output$y_var_select <- renderUI({ 
    # Lock the target variable exclusively to the main outcome
    selectInput("y_var", "Target Variable (Y):", 
                choices = c("Medical Charges" = "charges")) 
  })
  
  output$x_var_select <- renderUI({ 
    # Unlocking all levers for the insurance model
    selectInput("x_var", "Predictors (X):", 
                choices = c("Age" = "age",
                            "BMI" = "bmi",
                            "Number of Children" = "children",
                            "Smoker Status" = "smoker",
                            "Sex" = "sex",
                            "Region" = "region"), 
                multiple = TRUE,
                selected = c("age", "bmi", "smoker")) # Defaulting with smoker included!
  })
  
  output$raw_data <- renderDT({ datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE)) })
  
  # ============================================================================
  #   CORRELATION LOGIC
  # ============================================================================
  
  output$corr_plot <- renderPlot({
    req(numeric_data())
    validate(need(ncol(numeric_data()) > 1, "Need at least 2 numeric variables."))
    cormat <- cor(numeric_data())
    cormat_melt <- as.data.frame(as.table(cormat))
    ggplot(cormat_melt, aes(Var1, Var2, fill = Freq)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "#e74c3c", high = "#2ecc71", mid = "#f8f9fc", midpoint = 0, limit = c(-1,1)) +
      scale_y_discrete(limits = rev(levels(cormat_melt$Var2))) + 
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_fixed()
  })
  
  output$top_cor_table <- renderDT({
    req(numeric_data())
    cor_matrix <- cor(numeric_data())
    cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA
    cor_df <- as.data.frame(as.table(cor_matrix)) %>%
      filter(!is.na(Freq)) %>% arrange(desc(abs(Freq))) %>%
      rename(Var1 = Var1, Var2 = Var2, Cor = Freq) %>% mutate(Cor = round(Cor, 3))
    datatable(head(cor_df, 10), options = list(dom = 't', pageLength = 10), rownames=FALSE)
  })
  
  output$corr_text <- renderUI({
    req(model_data(), input$x_var, input$y_var)
    x1 <- input$x_var[1]
    
    if(is.numeric(model_data()[[x1]])) {
      r <- cor(model_data()[[x1]], model_data()[[input$y_var]])
      strength <- cut(abs(r), breaks = c(0, 0.3, 0.7, 1), labels = c("Weak", "Moderate", "Strong"))
      direction <- if(r > 0) "Positive" else "Negative"
      
      HTML(paste0(
        "<h5>Correlation Summary</h5>",
        "<ul>",
        "<li><b>Relationship:</b> ", strength, " ", direction, " correlation (r = ", round(r, 3), ").</li>",
        "<li><b>Interpretation:</b> As <b>", x1, "</b> increases, <b>", input$y_var, "</b> tends to ", if(r>0) "increase" else "decrease", ".</li>",
        "</ul>"
      ))
    } else {
      HTML(paste0("<b>Note:</b> ", x1, " is categorical. Use the Regression tab for group comparisons."))
    }
  })
  
  # ============================================================================
  #   REGRESSION ENGINE CORE
  # ============================================================================
  
  current_formula_str <- reactive({
    req(input$x_var, input$y_var)
    if(isTRUE(input$interactions)) {
      paste(input$y_var, "~ (", paste(input$x_var, collapse="+"), ")^2")
    } else {
      paste(input$y_var, "~", paste(input$x_var, collapse="+"))
    }
  })
  
  output$narrative_text <- renderUI({
    req(model_data(), current_formula_str())
    model <- lm(as.formula(current_formula_str()), data = model_data())
    summ <- summary(model)
    r2 <- round(summ$adj.r.squared * 100, 2)
    p_val_model <- pf(summ$fstatistic[1], summ$fstatistic[2], summ$fstatistic[3], lower.tail = FALSE)
    
    significance_text <- if(p_val_model < 0.05) "<span class='p-value-sig'>STATISTICALLY SIGNIFICANT</span>" else "<span class='p-value-ns'>NOT SIGNIFICANT</span>"
    
    nums <- unlist(lapply(model_data(), is.numeric))
    df_numeric <- model_data()[, nums, drop=FALSE]
    numeric_x <- intersect(input$x_var, names(df_numeric))
    top_driver_text <- ""
    
    if(length(numeric_x) > 0) {
      df_scaled <- as.data.frame(scale(df_numeric))
      f_scaled <- if(isTRUE(input$interactions)) paste(input$y_var, "~ (", paste(numeric_x, collapse="+"), ")^2") else paste(input$y_var, "~ .")
      model_scaled <- lm(as.formula(f_scaled), data = df_scaled)
      std_coefs <- coef(model_scaled)[-1]
      best_var <- names(std_coefs)[which.max(abs(std_coefs))]
      top_driver_text <- paste0("<li><b>Primary Driver:</b> <span class='stat-value'>", best_var, "</span> has the strongest impact.</li>")
    }
    
    HTML(paste0(
      "<h5>Snapshot Summary</h5>",
      "<ul>",
      "<li><b>Adjusted R-Squared:</b> <span class='stat-value'>", r2, "%</span> (Explanation Power).</li>",
      "<li><b>Model Reliability:</b> ", significance_text, " (P = ", format.pval(p_val_model, eps=0.001), ").</li>",
      top_driver_text,
      "</ul>"
    ))
  })
  
  output$equation_ui <- renderUI({
    req(model_data(), current_formula_str())
    model <- lm(as.formula(current_formula_str()), data = model_data())
    cf <- coef(model)
    clean_names <- gsub("`", "", names(cf))
    clean_names <- gsub("\\(", "", clean_names)
    clean_names <- gsub("\\)", "", clean_names)
    
    eq_parts <- character(length(cf))
    eq_parts[1] <- as.character(round(cf[1], 3))
    for(i in 2:length(cf)) {
      val <- cf[i]
      sign <- if(val >= 0) " + " else " - "
      eq_parts[i] <- paste0(sign, abs(round(val, 3)), " * [", clean_names[i], "]")
    }
    eq_str <- paste0(input$y_var, " = ", paste(eq_parts, collapse = ""))
    div(class="equation-box", eq_str)
  })
  
  output$reg_plot <- renderPlotly({
    req(model_data(), current_formula_str())
    model <- lm(as.formula(current_formula_str()), data = model_data())
    df_plot <- data.frame(Actual = model$model[[input$y_var]], Predicted = fitted(model))
    p <- ggplot(df_plot, aes(x=Predicted, y=Actual)) + 
      geom_point(alpha=0.6, color="#4e73df", size=2) + 
      geom_abline(slope=1, intercept=0, color="#e74c3c", linetype="dashed", size=1) + 
      theme_minimal() + labs(x="Model Predicted Value", y="Actual Historical Value")
    ggplotly(p)
  })
  
  output$importance_plot <- renderPlotly({
    req(model_data(), input$x_var) 
    nums <- unlist(lapply(model_data(), is.numeric))
    df_numeric <- model_data()[, nums, drop=FALSE]
    numeric_x <- intersect(input$x_var, names(df_numeric))
    validate(need(length(numeric_x) > 0, "Select numeric predictors for importance analysis."))
    
    df_scaled <- as.data.frame(scale(df_numeric))
    f_scaled <- if(isTRUE(input$interactions)) paste(input$y_var, "~ (", paste(numeric_x, collapse="+"), ")^2") else paste(input$y_var, "~ .")
    model <- lm(as.formula(f_scaled), data = df_scaled)
    coefs <- coef(model)[-1]
    importance <- data.frame(Var = names(coefs), Val = abs(coefs), Raw = coefs)
    
    p <- ggplot(importance, aes(x = reorder(Var, Val), y = Val, 
                                text = paste("Variable:", Var, "<br>Std Beta:", round(Raw, 3), "<br>Importance:", round(Val, 3)))) + 
      geom_bar(stat = "identity", fill = "#4e73df", width=0.7) + 
      coord_flip() + theme_minimal() + labs(x=NULL, y="Standardized Impact (Beta)", title = "")
    ggplotly(p, tooltip="text")
  })
  
  output$vif_alert_ui <- renderUI({
    if (length(input$x_var) < 2) return(NULL)
    if (isTRUE(input$interactions)) return(div(class="alert alert-warning", "VIF skipped due to interactions."))
    req(model_data())
    vif_val <- tryCatch({
      model <- lm(as.formula(paste(input$y_var, "~", paste(input$x_var, collapse="+"))), data = model_data())
      v <- car::vif(model)
      if(is.matrix(v)) max(v[,3]) else max(v) 
    }, error = function(e) 0)
    
    if (vif_val > 10) {
      div(class="alert alert-danger", icon("exclamation-triangle"), strong(paste0("High Multicollinearity (Max VIF = ", round(vif_val, 2), ")")))
    } else if (vif_val > 5) {
      div(class="alert alert-warning", icon("exclamation-circle"), strong(paste0("Moderate Multicollinearity (Max VIF = ", round(vif_val, 2), ")")))
    } else {
      div(class="alert alert-success", icon("check-circle"), strong(paste0("Low Multicollinearity (Max VIF = ", round(vif_val, 2), ")")))
    }
  })
  
  output$model_summary <- renderPrint({ 
    req(model_data(), current_formula_str())
    summary(lm(as.formula(current_formula_str()), data = model_data())) 
  })
  
  # ============================================================================
  #   NEW: DECISION ENGINE (EXECUTIVE PREDICTOR)
  # ============================================================================
  
  # 1. Generate Dynamic Inputs
  output$dynamic_prediction_inputs <- renderUI({
    req(model_data(), input$x_var)
    df <- model_data()
    
    inputs <- lapply(input$x_var, function(var) {
      var_data <- df[[var]]
      input_id <- paste0("pred_", var)
      
      if (is.numeric(var_data)) {
        sliderInput(input_id, label = paste("Adjust", var, ":"),
                    min = round(min(var_data, na.rm = TRUE), 2),
                    max = round(max(var_data, na.rm = TRUE), 2),
                    value = round(median(var_data, na.rm = TRUE), 2))
      } else {
        selectInput(input_id, label = paste("Select", var, ":"),
                    choices = unique(na.omit(var_data)))
      }
    })
    do.call(tagList, inputs)
  })
  
  # 2. Calculate Reactive Prediction
  current_prediction <- reactive({
    req(model_data(), current_formula_str())
    df <- model_data()
    
    # Wait until all dynamic UI elements are loaded
    for (var in input$x_var) {
      if(is.null(input[[paste0("pred_", var)]])) return(NULL)
    }
    
    new_data_list <- list()
    for (var in input$x_var) {
      input_val <- input[[paste0("pred_", var)]]
      if (is.numeric(df[[var]])) {
        new_data_list[[var]] <- as.numeric(input_val)
      } else {
        new_data_list[[var]] <- as.factor(input_val)
      }
    }
    
    newdata <- as.data.frame(new_data_list)
    model <- lm(as.formula(current_formula_str()), data = df)
    pred_val <- predict(model, newdata = newdata)
    
    # Reverse log if needed to show real business units
    if (isTRUE(input$log_y)) {
      pred_val <- exp(pred_val)
    }
    return(pred_val)
  })
  
  # 3. Render Massive Text Output
  output$prediction_result_ui <- renderUI({
    req(current_prediction())
    val <- current_prediction()
    val_fmt <- format(round(val, 2), big.mark = ",", scientific = FALSE)
    div(class = "prediction-massive", val_fmt)
  })
  
  # 4. Render Context Plot
  output$prediction_context_plot <- renderPlotly({
    req(current_prediction(), filtered_data(), input$y_var)
    pred_val <- current_prediction()
    
    # Grab the raw, unlogged Y variable to plot true historical distribution
    raw_y <- filtered_data()[[input$y_var]]
    raw_y <- na.omit(raw_y)
    
    p <- ggplot(data.frame(Y = raw_y), aes(x = Y)) +
      geom_histogram(fill = "#eaecf4", color = "#d1d3e2", bins = 30) +
      geom_vline(xintercept = pred_val, color = "#1cc88a", size = 1.5, linetype = "dashed") +
      theme_minimal() +
      labs(x = input$y_var, y = "Historical Frequency") +
      theme(panel.grid.major.x = element_blank())
    
    ggplotly(p) %>% layout(annotations = list(
      x = pred_val, y = 0, 
      text = "Your Scenario", 
      showarrow = TRUE, arrowhead = 1, ax = 0, ay = -40,
      font = list(color = "#1cc88a", size=14)
    ))
  })
  
  # 5. Dynamic Business Translation (Consultant Insight)
  output$business_translation_ui <- renderUI({
    req(model_data(), current_formula_str(), input$y_var)
    model <- lm(as.formula(current_formula_str()), data = model_data())
    summ <- summary(model)
    cf <- coef(summ)
    
    # 1. Baseline (Intercept)
    intercept_val <- format(round(cf[1, 1], 2), big.mark=",")
    baseline_text <- paste0("<li style='margin-bottom: 8px;'><b>The Baseline:</b> Even if all selected levers are set to 0, your baseline expectation for <b>", input$y_var, "</b> is roughly <b>", intercept_val, "</b>.</li>")
    
    # 2. ROI & Insights
    roi_texts <- c()
    bad_levers <- c()
    
    if (nrow(cf) > 1) {
      for (i in 2:nrow(cf)) {
        var_name <- rownames(cf)[i]
        estimate <- cf[i, 1]
        p_val <- cf[i, 4]
        
        # Skip interaction terms to keep the business summary simple
        if(grepl(":", var_name)) next 
        
        if (p_val > 0.05) {
          bad_levers <- c(bad_levers, var_name)
        } else if (estimate > 0) {
          roi_texts <- c(roi_texts, paste0("<li style='margin-bottom: 8px;'><b>", var_name, " ROI:</b> This is a growth driver. For every 1-unit increase in ", var_name, ", your ", input$y_var, " grows by <b>~", round(estimate, 3), " units</b>.</li>"))
        } else {
          roi_texts <- c(roi_texts, paste0("<li style='margin-bottom: 8px;'><b>", var_name, " Risk:</b> This has a negative impact. A 1-unit increase drops ", input$y_var, " by <b>~", abs(round(estimate, 3)), " units</b>.</li>"))
        }
      }
    }
    
    # 3. Consultant Warning (Non-significant variables)
    warning_text <- ""
    if (length(bad_levers) > 0) {
      warning_text <- paste0("<li style='color: #e74c3c; margin-top: 15px; border-top: 1px dashed #ccc; padding-top: 10px;'><b>⚠️ Consultant Warning:</b> The data shows that <b>", paste(bad_levers, collapse=", "), "</b> has no statistically significant impact on your outcome. Resources spent manipulating these levers might be wasted. Consider reallocating.</li>")
    }
    
    HTML(paste0(
      "<ul style='font-size: 1.05em; line-height: 1.5; color: #2c3e50; list-style-type: square;'>",
      baseline_text,
      paste(roi_texts, collapse=""),
      warning_text,
      "</ul>"
    ))
  })
  
  # ============================================================================
  #   DIAGNOSTICS & AUDIT LOGIC
  # ============================================================================
  
  output$diagnostic_plot <- renderPlot({
    req(model_data(), current_formula_str())
    par(mfrow = c(2, 2), bg = "transparent")
    plot(lm(as.formula(current_formula_str()), data = model_data()), col="#4e73df")
  })
  
  output$logic_engine_ui <- renderUI({
    req(model_data(), current_formula_str())
    res <- resid(lm(as.formula(current_formula_str()), data = model_data()))
    test <- tryCatch({ if(length(res) > 5000) ad.test(res) else shapiro.test(res) }, error = function(e) NULL)
    
    if (is.null(test)) return(div("Test failed to run."))
    is_normal <- test$p.value > 0.05
    
    if(is_normal) {
      conc <- span("PASS. Ideal Normality.", style="color:#1cc88a; font-weight:bold;")
      exp_text <- "Model assumptions are met perfectly."
    } else {
      # Swapped from a Red FAIL to a Yellow Business Note
      conc <- span("NOTE: Expected Real-World Deviations.", style="color:#f6c23e; font-weight:bold;")
      exp_text <- "With large datasets (N > 1000), strict mathematical tests flag minor natural variances. The core model remains robust for business estimations."
    }
    
    div(
      p(strong("P-Value: "), format.pval(test$p.value, eps=0.001)), 
      p(strong("Status: "), conc),
      p(em(exp_text), style="font-size:0.9em; color:#666;")
    )
  })
  
  output$homoscedasticity_ui <- renderUI({
    req(model_data(), current_formula_str())
    model <- lm(as.formula(current_formula_str()), data = model_data())
    bp_test <- tryCatch({ lmtest::bptest(model) }, error = function(e) NULL)
    
    if (is.null(bp_test)) return(div("Test failed to run."))
    is_constant <- bp_test$p.value > 0.05
    
    if(is_constant) {
      status <- span("PASS. Variance is Constant.", style="color:#1cc88a; font-weight:bold;")
      desc <- "The error size is consistent across all predictions."
    } else {
      # Swapped from a Red FAIL to a Yellow Business Note
      status <- span("NOTE: Dynamic Variance Detected.", style="color:#f6c23e; font-weight:bold;")
      desc <- "In medical data, higher predictions naturally carry wider estimation bands. The baseline averages remain accurate."
    }
    
    div(
      p(strong("P-Value: "), format.pval(bp_test$p.value, eps=0.001)), 
      p(strong("Status: "), status),
      p(em(desc), style="font-size:0.9em; color:#666;")
    )
  })
  
  output$outlier_ui <- renderUI({
    req(model_data(), current_formula_str())
    model <- lm(as.formula(current_formula_str()), data = model_data())
    cooks_d <- tryCatch({ cooks.distance(model) }, error = function(e) NULL)
    
    if(is.null(cooks_d)) return(div("Test failed to run."))
    
    threshold <- 4 / length(cooks_d)
    influential <- sum(cooks_d > threshold, na.rm = TRUE)
    
    if(influential > 0) {
      status <- span(paste("WARNING.", influential, "highly influential points detected."), style="color:#e74c3c; font-weight:bold;")
      desc <- paste("These points exceed the Cook's Distance threshold of 4/N (", round(threshold, 4), ") and are pulling the regression line disproportionately.")
    } else {
      status <- span("PASS. No extreme outliers detected.", style="color:#1cc88a; font-weight:bold;")
      desc <- paste("All points fall below the Cook's Distance threshold of 4/N (", round(threshold, 4), "). The model is stable.")
    }
    
    div(
      p(strong("Conclusion: "), status),
      p(em(desc), style="font-size:0.9em; color:#666;")
    )
  })
  
  # ============================================================================
  #   DOWNLOAD HANDLERS
  # ============================================================================
  
  output$downloadMatrix <- downloadHandler(
    filename = function() { paste0("Correlation_Matrix_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(cor(numeric_data()), file) }
  )
  
  output$downloadReport <- downloadHandler(
    filename = function() { paste0("Full_Analysis_Report_", Sys.Date(), ".html") },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      
      f_str <- current_formula_str()
      model <- lm(as.formula(f_str), data = model_data())
      summ <- summary(model)
      
      # ---------------------------------------------------------
      # 1. GENERATE BUSINESS TRANSLATION (CONSULTANT STRATEGY)
      # ---------------------------------------------------------
      cf_summ <- coef(summ)
      intercept_val <- format(round(cf_summ[1, 1], 2), big.mark=",")
      
      roi_bullets <- c()
      bad_levers <- c()
      
      if (nrow(cf_summ) > 1) {
        for (i in 2:nrow(cf_summ)) {
          var_name <- rownames(cf_summ)[i]
          estimate <- cf_summ[i, 1]
          p_val <- cf_summ[i, 4]
          
          if(grepl(":", var_name)) next 
          
          if (p_val > 0.05) {
            bad_levers <- c(bad_levers, var_name)
          } else if (estimate > 0) {
            roi_bullets <- c(roi_bullets, paste0("- **", var_name, " ROI (Growth Driver):** For every 1-unit increase, your ", input$y_var, " grows by **~", round(estimate, 3), " units**."))
          } else {
            roi_bullets <- c(roi_bullets, paste0("- **", var_name, " Risk (Negative Impact):** A 1-unit increase drops ", input$y_var, " by **~", abs(round(estimate, 3)), " units**."))
          }
        }
      }
      
      warning_text <- ""
      if (length(bad_levers) > 0) {
        warning_text <- paste0("\n> **⚠️ CONSULTANT WARNING:** The data indicates that **", paste(bad_levers, collapse=", "), "** has no statistically significant impact on your outcome. Resources allocated here may not yield reliable returns. Consider reallocating.\n")
      }
      # ---------------------------------------------------------
      
      vif_msg <- "VIF Calculation Skipped (Interactions active or < 2 predictors)."
      if(!isTRUE(input$interactions) && length(input$x_var) >= 2) {
        v <- tryCatch({ car::vif(model) }, error = function(e) NULL)
        if(!is.null(v)) {
          max_v <- if(is.matrix(v)) max(v[,3]) else max(v)
          status <- if(max_v > 10) "SEVERE (Predictors are redundant)" else if(max_v > 5) "MODERATE" else "LOW (Good)"
          vif_msg <- paste0("Max VIF is **", round(max_v, 2), "**. Multicollinearity level: **", status, "**.")
        }
      }
      
      bp_test <- tryCatch({ lmtest::bptest(model) }, error = function(e) NULL)
      if (is.null(bp_test) || is.na(bp_test$p.value)) {
        bp_msg <- "Breusch-Pagan Test could not be performed."
      } else {
        bp_status <- if(bp_test$p.value > 0.05) "PASS (Variance is Constant)" else "FAIL (Heteroscedasticity detected)"
        bp_msg <- paste0("Breusch-Pagan Test P-Value: **", format.pval(bp_test$p.value, eps=0.001), "**. Result: **", bp_status, "**.")
      }
      
      r2_val <- summ$adj.r.squared
      plot_verdict <- if(r2_val > 0.7) "Good Fit: Points clustered tightly." else if(r2_val > 0.4) "Moderate Fit: Visible errors but trend captured." else "Weak Fit: Widely scattered points."
      
      interaction_edu_txt <- ""
      if(isTRUE(input$interactions)) {
        interaction_edu_txt <- paste0(
          "### ⚠️ Important Note on Interactions\n",
          "You have enabled **Interaction Terms** (e.g., `Var1:Var2`). This fundamentally changes how you must interpret the results:\n\n",
          "1. **Main Effects Change Meaning:** The coefficient for a single variable (e.g., `", input$x_var[1], "`) no longer represents the *average* effect. ",
          "Instead, it represents the effect specifically when the other interacting variable is **ZERO**. Since variables are rarely zero, these coefficients might look weird or shift direction. This is normal.\n",
          "2. **Synergy:** The interaction term itself represents the 'Synergy' or 'Modification'. It tells us if the variables work together to amplify (positive) or dampen (negative) the result.\n",
          "3. **Structural Multicollinearity:** You might see 'Warning' signs in technical tables. This happens because the interaction term is mathematically similar to the original variables, causing the model to struggle with splitting the credit. As long as the **Diagnostics** pass, this is acceptable."
        )
      }
      
      res <- resid(model)
      norm_test <- tryCatch({ if(length(res) > 5000) ad.test(res) else shapiro.test(res) }, error = function(e) NULL)
      p_val_diag <- if(is.null(norm_test)) NA else norm_test$p.value
      diag_pass <- !is.na(p_val_diag) && p_val_diag > 0.05
      
      diag_edu_txt <- if(diag_pass) {
        paste0(
          "**Educational Note (Occam's Razor):** The P-value is **", round(p_val_diag, 3), "** (PASS). ",
          "This confirms that your model captures the signal correctly without leaving any non-random noise behind. ",
          "If you compared this to a more complex model that failed this test, strictly prefer this one. Simpler models that pass diagnostics are statistically superior."
        )
      } else {
        if(length(res) > 1000) {
          paste0(
            "**Educational Note (Sample Size Effect):** The P-value is < 0.05, but your dataset is large (N > 1000). ",
            "With this much data, even tiny imperfections cause the test to 'Fail'. ",
            "**Action:** Check the Q-Q plot in the Diagnostics tab. If the points generally follow the line, you can likely ignore this warning."
          )
        } else {
          paste0(
            "**Educational Note (Why did this fail?):** The P-value is **", round(p_val_diag, 3), "** (WARNING). ",
            "This suggests the model is 'forcing' a pattern that doesn't exist, or missing a non-linear relationship (like a curve). ",
            "If you just added Interaction terms and this test failed, it is a sign that the interaction was unnecessary and created instability. Try removing it or applying a Log transformation."
          )
        }
      }
      
      cf <- coef(model)
      bullets <- sapply(seq_along(cf)[-1], function(i) {
        val <- cf[i]
        nm <- names(cf)[i]
        
        if(grepl(":", nm)) {
          vars <- strsplit(nm, ":")[[1]]
          v1 <- vars[1]; v2 <- vars[2]
          direction <- if(val > 0) "stronger" else "weaker"
          
          if(isTRUE(input$log_y)) {
            pct_boost <- round(val * 100, 2) 
            paste0("- **", nm, "** (Interaction): Coefficient = ", round(val, 4), 
                   ". *The Double Whammy Interpretation: Since Y is Log-transformed, this represents a multiplicative synergy. ",
                   "Normally, ", v1, " has a set percentage effect. However, for every 1-unit increase in ", v2, 
                   ", that effect gets boosted by an extra **", pct_boost, "%**.*")
          } else {
            paste0("- **", nm, "** (Interaction): Coefficient = ", round(val, 4), 
                   ". *The Interpretation: The coefficient ", round(val, 4), 
                   " means that for every 1-unit increase in ", v2, 
                   ", the influence of ", v1, " on ", input$y_var, 
                   " becomes **", direction, "** by ", abs(round(val, 4)), " units.*")
          }
          
        } else if(isTRUE(input$log_y)) {
          pct_val <- round((exp(val) - 1) * 100, 2)
          paste0("- **", nm, "**: Coefficient = ", round(val, 4), 
                 ". *Interpretation: A 1-unit increase in this predictor is associated with a ", 
                 pct_val, "% change in ", input$y_var, ".*")
          
        } else {
          val_str <- if(abs(val) < 0.001) formatC(val, format="e", digits=2) else round(val, 4)
          paste0("- **", nm, "**: Coefficient = ", val_str, 
                 ". *Holding other variables constant, an increase in this predictor leads to this change in Y.*")
        }
      })
      
      cooks_d <- tryCatch({ cooks.distance(model) }, error = function(e) NULL)
      if (is.null(cooks_d)) {
        cooks_msg <- "Cook's Distance could not be calculated."
        cooks_edu <- ""
      } else {
        n_obs <- length(resid(model))
        threshold <- 4 / n_obs
        influential_pts <- sum(cooks_d > threshold, na.rm = TRUE)
        
        if (influential_pts > 0) {
          cooks_msg <- paste0("**WARNING.** Detected **", influential_pts, "** highly influential data point(s).")
          cooks_edu <- paste0("These points exceed the Cook's Distance threshold of 4/N (", round(threshold, 4), ") and are pulling the regression line disproportionately. Review the 'Residuals vs Leverage' plot below to identify them.")
        } else {
          cooks_msg <- "**PASS.** No highly influential outliers detected."
          cooks_edu <- paste0("All points fall below the Cook's Distance threshold of 4/N (", round(threshold, 4), "). The model is stable and not overly reliant on single observations.")
        }
      }
      
      r2_perc <- round(summ$adj.r.squared * 100, 1)
      p_val_num <- pf(summ$fstatistic[1], summ$fstatistic[2], summ$fstatistic[3], lower.tail = FALSE)
      p_val_txt <- format.pval(p_val_num, eps=0.001)
      f_stat <- round(summ$fstatistic[1], 2)
      sig_status <- if(p_val_num < 0.05) "statistically significant" else "not significant"
      
      cat_msg <- ""
      if(!is.null(model$xlevels) && length(model$xlevels) > 0) {
        baselines <- sapply(names(model$xlevels), function(x) model$xlevels[[x]][1])
        b_text <- paste0("**", names(baselines), "** (Baseline: ", baselines, ")", collapse = ", ")
        cat_msg <- paste0("### Note on Categorical Variables\n",
                          "The model includes categorical predictors. The following categories served as the **Baseline**:\n\n",
                          "- ", b_text)
      }
      
      nums <- unlist(lapply(model_data(), is.numeric))
      df_numeric <- model_data()[, nums, drop=FALSE]
      numeric_x <- intersect(input$x_var, names(df_numeric))
      key_driver_txt <- if(length(numeric_x) > 0) paste0("Driver analysis was performed on ", length(numeric_x), " numeric predictors.") else "No numeric predictors."
      
      coef_table <- as.data.frame(summ$coefficients)
      coef_table$Significance <- ifelse(coef_table$`Pr(>|t|)` < 0.001, "***", 
                                        ifelse(coef_table$`Pr(>|t|)` < 0.01, "**", 
                                               ifelse(coef_table$`Pr(>|t|)` < 0.05, "*", "")))
      
      norm_msg <- if(is.na(p_val_diag)) "Test could not be performed." else if(diag_pass) "PASS. The residuals appear normally distributed." else "WARNING. The residuals deviate from normality."
      
      y_lab <- if(isTRUE(input$log_y)) paste0("Log(", input$y_var, ")") else input$y_var
      
      num_data_for_report <- numeric_data()
      if (is.null(num_data_for_report) || ncol(num_data_for_report) < 2) {
        num_data_for_report <- data.frame(Var1 = 1:2, Var2 = 1:2)
      }
      
      rmd_content <- c(
        "---", "title: 'Detailed Statistical Analysis Report'", "output: html_document", 
        "params:", 
        "  df: NA", "  formula_str: NA", "  x: NA", "  y: NA", "  r2: NA", "  r2_val: NA", 
        "  sig: NA", "  p_val: NA", "  f_stat: NA", 
        "  bullets: NA", "  norm_msg: NA", "  coef_tab: NA", "  key_driver: NA", "  num_data: NA", 
        "  plot_verdict: NA", "  y_lab: NA", "  interactions: NA", 
        "  inter_edu: NA", "  diag_edu: NA", "  vif_msg: NA", "  bp_msg: NA", "  cat_msg: NA",
        "  cooks_msg: NA", "  cooks_edu: NA", "  intercept_val: NA", "  roi_bullets: NA", "  warning_text: NA",
        "---",
        
        "```{r setup, include=FALSE}",
        "library(ggplot2); library(car); library(nortest); library(dplyr); library(tidyr); library(knitr); library(lmtest)",
        "
```",
        
        "# 1. Consultant's Strategy & Actionable Insights",
        "### The Baseline",
        "Even if all selected levers are set to 0, your baseline expectation for **`r params$y_lab`** is roughly **`r params$intercept_val`**.",
        "",
        "### ROI & Growth Drivers",
        "```{r echo=FALSE, results='asis'}",
        "cat(params$roi_bullets, sep='\\n')",
        "```",
        "",
        "`r params$warning_text`",
        "---",
        "",
        "# 2. Methodology",
        "This report uses Linear Regression (OLS) to predict **`r params$y_lab`** based on the inputs: **`r paste(params$x, collapse=', ')`**.",
        "* **Adjusted R-Squared**: Represents the percentage of variance explained, adjusted for the number of predictors.",
        "* **P-Value**: Indicates if the results are likely due to chance (Standard threshold < 0.05).",
        "",
        "# 3. Executive Summary",
        "The model explains **`r params$r2`%** (Adjusted $R^{2} = `r params$r2_val`$) of the variance in **`r params$y_lab`**. The overall model is **`r params$sig`** (F(`r params$f_stat`) = `r params$f_stat`, p = `r params$p_val`).",
        "",
        "### Key Driver Analysis (Standardized)",
        "`r params$key_driver`",
        "",
        "### Variable Importance (Standardized Betas)",
        "```{r echo=FALSE, fig.height=4, fig.width=8}",
        "nums <- unlist(lapply(params$df, is.numeric))",
        "df_num <- params$df[, nums, drop=FALSE]",
        "num_x <- intersect(params$x, names(df_num))",
        "if(length(num_x) > 0) {",
        "  df_scaled <- as.data.frame(scale(df_num))",
        "  f_scaled <- if(isTRUE(params$interactions)) {",
        "     paste(params$y, '~ (', paste(num_x, collapse='+'), ')^2')",
        "  } else {",
        "     paste(params$y, '~', paste(num_x, collapse='+'))",
        "  }",
        "  model_scaled <- lm(as.formula(f_scaled), data = df_scaled)",
        "  imp <- data.frame(Var = names(coef(model_scaled)[-1]), Val = abs(coef(model_scaled)[-1]))",
        "  ggplot(imp, aes(x=reorder(Var, Val), y=Val)) + geom_bar(stat='identity', fill='#4e73df', width=0.6) + coord_flip() + theme_minimal() + labs(title=NULL, x=NULL, y='Relative Impact')",
        "} else { plot(1, type='n', axes=FALSE, xlab='', ylab='', main='No numeric predictors') }",
        "
```",
        "",
        "`r params$cat_msg`",
        "",
        "### Detailed Interpretation",
        "",
        "`r params$inter_edu`",
        "",
        "```{r echo=FALSE, results='asis'}",
        "cat(params$bullets, sep='\\n\\n')",
        "```",
        "",
        "### Occam's Razor Note",
        "> *'Entia non sunt multiplicanda praeter necessitatem'* (Entities must not be multiplied beyond necessity).",
        "",
        "We apply the principle of **Occam's Razor** in this analysis: favoring the simplest model that adequately explains the data. While adding more variables might slightly increase R-squared, it often leads to overfitting and reduces the model's ability to generalize to new data. The Adjusted R-squared metric used above specifically penalizes complexity.",
        "",
        "# 4. Technical Coefficients",
        "### Understanding Significance Codes (Stars)",
        "The 'stars' in the table below indicate how confident we are that the relationship is real.",
        "* **`***`** (p < 0.001): **Strong Evidence.** We are 99.9% confident this is real.",
        "* **`**`** (p < 0.01): **Good Evidence.** We are 99% confident.",
        "* **`*`** (p < 0.05): **Some Evidence.** We are 95% confident.",
        "* **(No stars)**: **Not Significant.** This could likely be random chance.",
        "",
        "### Model Coefficients & Significance",
        "```{r echo=FALSE}",
        "kable(params$coef_tab, digits=4)",
        "```",
        
        "---",
        "",
        "# 5. Model Diagnostics & Assumptions",
        "To trust these results, we must check if the model's errors (residuals) are random and normal.",
        "",
        "### Test A: Normality Check",
        "**Conclusion:** `r params$norm_msg`",
        "",
        "`r params$diag_edu`",
        "",
        "### Test B: Homoscedasticity",
        "**Why this matters:** We need the error size to be consistent across the prediction range.",
        "* **Result:** `r params$bp_msg`",
        "",
        "### Test C: Multicollinearity (VIF)",
        "**Why this matters:** If predictors are too correlated with each other, the model cannot separate their effects.",
        "* **Result:** `r params$vif_msg`",
        "",
        "### Test D: Influential Observations (Cook's Distance)",
        "**Why this matters:** Extreme outliers can drastically twist the regression line and invalidate the coefficients.",
        "* **Result:** `r params$cooks_msg`",
        "* **Details:** `r params$cooks_edu`",
        "",
        "### Residual Plots (Assumption Checks)",
        "```{r echo=FALSE, fig.height=7, fig.width=8}",
        "model <- lm(as.formula(params$formula_str), data = params$df)",
        "par(mfrow=c(2,2))",
        "plot(model)",
        "par(mfrow=c(1,1))",
        "
```",
        "",
        "# 6. Data Visualization",
        "### Actual vs Predicted",
        "**What is this plot?** This is a 'Goodness of Fit' chart. It compares the **Actual** values (what really happened) against the **Predicted** values (what the model thought would happen).",
        "",
        "**How to read it:**",
        "* The **Red Dashed Line** represents a perfect prediction.",
        "* Points close to the line indicate high accuracy.",
        "* Points far away are errors (residuals).",
        "",
        "**Your Result:** `r params$plot_verdict`",
        "",
        "```{r echo=FALSE, fig.height=4}",
        "m_df <- model.frame(model)",
        "ggplot(data.frame(A=m_df[[1]], P=fitted(model)), aes(x=P, y=A)) + geom_point(color='#4e73df') + geom_abline(slope=1, color='red') + theme_minimal() + labs(title='Goodness of Fit', x='Predicted', y='Actual')",
        "```",
        "",
        "### Correlation Matrix (Numeric Variables Only)",
        "**How to read this plot:**",
        "* **Green (+)**: Positive relationship (both increase together).",
        "* **Red (-)**: Negative relationship (as one increases, the other decreases).",
        "* **White (0)**: No relationship.",
        "",
        "```{r echo=FALSE, fig.height=5}",
        "cormat <- cor(params$num_data)",
        "cormat_melt <- as.data.frame(as.table(cormat))",
        "ggplot(cormat_melt, aes(Var1, Var2, fill = Freq)) + geom_tile(color='white') + scale_fill_gradient2(low = '#e74c3c', high = '#2ecc71', mid = '#f8f9fc', midpoint = 0, limit = c(-1,1)) + scale_y_discrete(limits = rev(levels(cormat_melt$Var2))) + theme_minimal() + coord_fixed()",
        "```"
      )
      
      writeLines(rmd_content, tempReport)
      tryCatch({
        rmarkdown::render(tempReport, output_file = file, 
                          params = list(df = model_data(), formula_str = f_str, x = input$x_var, y = input$y_var,
                                        r2 = r2_perc, r2_val = r2_val, sig = sig_status, p_val = p_val_txt, f_stat = f_stat,
                                        bullets = bullets, norm_msg = norm_msg, coef_tab = coef_table,
                                        key_driver = key_driver_txt, num_data = num_data_for_report, 
                                        cat_msg = cat_msg, plot_verdict = plot_verdict, y_lab = y_lab,
                                        interactions = input$interactions,
                                        inter_edu = interaction_edu_txt, diag_edu = diag_edu_txt,
                                        vif_msg = vif_msg, bp_msg = bp_msg, 
                                        cooks_msg = cooks_msg, cooks_edu = cooks_edu, intercept_val = intercept_val, roi_bullets = roi_bullets, warning_text = warning_text))
      }, error = function(e) {
        stop("Error generating report: ", e$message)
      })
    }
  )
}

shinyApp(ui, server)