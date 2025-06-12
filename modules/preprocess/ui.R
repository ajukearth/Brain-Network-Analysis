#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/preprocess/ui.R - Preprocessing module UI components
#########################################################

# Preprocessing module UI
tagList(
  fluidRow(
    column(
      width = 12,
      box(
        title = "Data Preprocessing",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        
        p("Prepare your data for network analysis by handling missing values, outliers, and applying transformations."),
        
        conditionalPanel(
          condition = "!output.import_complete",
          div(
            class = "alert alert-warning",
            icon("exclamation-triangle"),
            "Please complete the Data Import step first."
          )
        )
      )
    )
  ),
  
  conditionalPanel(
    condition = "output.import_complete",
    fluidRow(
      column(
        width = 12,
        tabBox(
          id = "preprocess_tabs",
          width = NULL,
          
          # Missing Data tab
          tabPanel(
            title = "Missing Data",
            icon = icon("search-minus"),
            
            fluidRow(
              column(
                width = 8,
                box(
                  title = "Missing Data Visualization",
                  width = NULL,
                  plotOutput("preprocess_missing_plot", height = "400px")
                )
              ),
              
              column(
                width = 4,
                box(
                  title = "Missing Data Summary",
                  width = NULL,
                  verbatimTextOutput("preprocess_missing_summary")
                )
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                box(
                  title = "Missing Data Handling",
                  width = NULL,
                  
                  conditionalPanel(
                    condition = "output.has_missing_data",
                    
                    fluidRow(
                      column(
                        width = 6,
                        selectInput(
                          "preprocess_missing_method",
                          "Imputation Method:",
                          choices = c(
                            "None (exclude incomplete cases)" = "none",
                            "Mean Imputation" = "mean",
                            "Median Imputation" = "median",
                            "K-Nearest Neighbors" = "knn",
                            "Multiple Imputation (MICE)" = "mice"
                          ),
                          selected = "mean"
                        )
                      ),
                      
                      column(
                        width = 6,
                        conditionalPanel(
                          condition = "input.preprocess_missing_method == 'knn'",
                          sliderInput(
                            "preprocess_knn_k",
                            "Number of Neighbors (k):",
                            min = 1,
                            max = 20,
                            value = 5,
                            step = 1
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.preprocess_missing_method == 'mice'",
                          selectInput(
                            "preprocess_mice_method",
                            "MICE Method:",
                            choices = c(
                              "Predictive Mean Matching" = "pmm",
                              "Bayesian Linear Regression" = "norm",
                              "Random Forest" = "rf"
                            ),
                            selected = "pmm"
                          )
                        )
                      )
                    ),
                    
                    fluidRow(
                      column(
                        width = 6,
                        checkboxInput(
                          "preprocess_exclude_high_missing_regions",
                          "Exclude regions with high missing data",
                          value = FALSE
                        ),
                        
                        conditionalPanel(
                          condition = "input.preprocess_exclude_high_missing_regions",
                          sliderInput(
                            "preprocess_region_missing_threshold",
                            "Region exclusion threshold (%):",
                            min = 0,
                            max = 50,
                            value = 20,
                            step = 1
                          )
                        )
                      ),
                      
                      column(
                        width = 6,
                        checkboxInput(
                          "preprocess_exclude_high_missing_subjects",
                          "Exclude subjects with high missing data",
                          value = FALSE
                        ),
                        
                        conditionalPanel(
                          condition = "input.preprocess_exclude_high_missing_subjects",
                          sliderInput(
                            "preprocess_subject_missing_threshold",
                            "Subject exclusion threshold (%):",
                            min = 0,
                            max = 50,
                            value = 20,
                            step = 1
                          )
                        )
                      )
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "!output.has_missing_data",
                    div(
                      class = "alert alert-success",
                      icon("check-circle"),
                      "No missing data detected in the dataset."
                    )
                  )
                )
              )
            )
          ),
          
          # Outliers tab
          tabPanel(
            title = "Outliers",
            icon = icon("filter"),
            
            fluidRow(
              column(
                width = 8,
                box(
                  title = "Data Distribution",
                  width = NULL,
                  
                  selectInput(
                    "preprocess_outlier_plot_type",
                    "Plot Type:",
                    choices = c(
                      "Boxplots" = "boxplot",
                      "Histograms" = "histogram",
                      "Density Plots" = "density",
                      "QQ Plots" = "qqplot"
                    ),
                    selected = "boxplot"
                  ),
                  
                  plotOutput("preprocess_outlier_plot", height = "400px")
                )
              ),
              
              column(
                width = 4,
                box(
                  title = "Outlier Detection",
                  width = NULL,
                  
                  selectInput(
                    "preprocess_outlier_method",
                    "Detection Method:",
                    choices = c(
                      "None" = "none",
                      "IQR Method" = "iqr",
                      "Z-Score Method" = "zscore",
                      "Modified Z-Score (MAD)" = "mad"
                    ),
                    selected = "iqr"
                  ),
                  
                  conditionalPanel(
                    condition = "input.preprocess_outlier_method != 'none'",
                    
                    sliderInput(
                      "preprocess_outlier_threshold",
                      "Threshold:",
                      min = 1,
                      max = 5,
                      value = 2.5,
                      step = 0.1
                    ),
                    
                    selectInput(
                      "preprocess_outlier_handling",
                      "Handling Method:",
                      choices = c(
                        "Flag Only (No Changes)" = "flag",
                        "Winsorize (Cap at Threshold)" = "winsorize",
                        "Replace with NA" = "na",
                        "Remove Outlier Cases" = "remove"
                      ),
                      selected = "flag"
                    )
                  ),
                  
                  verbatimTextOutput("preprocess_outlier_summary")
                )
              )
            )
          ),
          
          # Transformations tab
          tabPanel(
            title = "Transformations",
            icon = icon("exchange-alt"),
            
            fluidRow(
              column(
                width = 8,
                box(
                  title = "Data Transformation Preview",
                  width = NULL,
                  
                  fluidRow(
                    column(
                      width = 6,
                      selectInput(
                        "preprocess_transform_region",
                        "Select Region to Preview:",
                        choices = NULL
                      )
                    ),
                    
                    column(
                      width = 6,
                      selectInput(
                        "preprocess_transform_plot_type",
                        "Plot Type:",
                        choices = c(
                          "Histogram" = "histogram",
                          "Density" = "density",
                          "QQ Plot" = "qqplot"
                        ),
                        selected = "histogram"
                      )
                    )
                  ),
                  
                  plotOutput("preprocess_transform_plot", height = "400px")
                )
              ),
              
              column(
                width = 4,
                box(
                  title = "Transformation Options",
                  width = NULL,
                  
                  selectInput(
                    "preprocess_transform_method",
                    "Transformation Method:",
                    choices = c(
                      "None" = "none",
                      "Z-Score Standardization" = "zscore",
                      "Min-Max Scaling" = "minmax",
                      "Log Transformation" = "log",
                      "Square Root Transformation" = "sqrt",
                      "Box-Cox Transformation" = "boxcox"
                    ),
                    selected = "none"
                  ),
                  
                  conditionalPanel(
                    condition = "input.preprocess_transform_method == 'log'",
                    checkboxInput(
                      "preprocess_log_plus_one",
                      "Add 1 before log (log(x+1))",
                      value = TRUE
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "input.preprocess_transform_method == 'boxcox'",
                    sliderInput(
                      "preprocess_boxcox_lambda",
                      "Lambda Parameter:",
                      min = -2,
                      max = 2,
                      value = 0,
                      step = 0.1
                    ),
                    checkboxInput(
                      "preprocess_boxcox_auto",
                      "Auto-select optimal lambda",
                      value = TRUE
                    )
                  ),
                  
                  checkboxInput(
                    "preprocess_transform_by_group",
                    "Apply transformation within each group",
                    value = FALSE
                  ),
                  
                  hr(),
                  
                  selectInput(
                    "preprocess_transform_scope",
                    "Apply To:",
                    choices = c(
                      "All Brain Regions" = "all",
                      "Selected Regions" = "selected"
                    ),
                    selected = "all"
                  ),
                  
                  conditionalPanel(
                    condition = "input.preprocess_transform_scope == 'selected'",
                    selectizeInput(
                      "preprocess_transform_regions",
                      "Select Regions:",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(plugins = list("remove_button"))
                    )
                  ),
                  
                  actionButton(
                    "preprocess_apply_transform",
                    "Apply Transformation",
                    icon = icon("sync"),
                    class = "btn-primary"
                  )
                )
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                box(
                  title = "Normality Tests",
                  width = NULL,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  
                  DT::DTOutput("preprocess_normality_tests")
                )
              )
            )
          ),
          
          # Quality Checks tab
          tabPanel(
            title = "Quality Checks",
            icon = icon("check-square"),
            
            fluidRow(
              column(
                width = 12,
                box(
                  title = "Data Quality Checks",
                  width = NULL,
                  
                  fluidRow(
                    column(
                      width = 6,
                      h4("Variance Check"),
                      p("Regions with low variance may not contribute meaningfully to network analysis."),
                      DT::DTOutput("preprocess_variance_check")
                    ),
                    
                    column(
                      width = 6,
                      h4("Correlation Check"),
                      p("Highly correlated regions may introduce redundancy in the network."),
                      DT::DTOutput("preprocess_correlation_check")
                    )
                  ),
                  
                  hr(),
                  
                  fluidRow(
                    column(
                      width = 12,
                      h4("Summary Statistics by Region"),
                      DT::DTOutput("preprocess_region_summary")
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        box(
          title = "Preprocessing Results",
          width = NULL,
          status = "success",
          solidHeader = TRUE,
          
          fluidRow(
            column(
              width = 8,
              verbatimTextOutput("preprocess_summary")
            ),
            
            column(
              width = 4,
              div(
                style = "display: flex; flex-direction: column; gap: 10px;",
                
                actionButton(
                  "preprocess_reset",
                  "Reset All Preprocessing",
                  icon = icon("undo"),
                  class = "btn-warning btn-block"
                ),
                
                actionButton(
                  "preprocess_proceed_btn",
                  "Save & Proceed to Correlation Methods",
                  icon = icon("arrow-right"),
                  class = "btn-success btn-block"
                )
              )
            )
          )
        )
      )
    )
  )
)
