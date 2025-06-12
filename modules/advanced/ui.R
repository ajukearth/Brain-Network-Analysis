#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/advanced/ui.R - Advanced analysis UI
#########################################################

# Define advanced analysis UI
fluidPage(
  # Initial message based on visualization state
  uiOutput("advanced_initial_message"),
  
  # Main advanced analysis UI - only shown when visualization is complete
  conditionalPanel(
    condition = "output.visualization_complete",
    
    fluidRow(
      column(
        width = 12,
        h3("Advanced Network Analysis"),
        p("Perform advanced analysis on your brain network data to gain deeper insights."),
        hr()
      )
    ),
    
    fluidRow(
      # Tabs for different advanced analyses
      column(
        width = 12,
        tabBox(
          width = NULL,
          id = "advanced_analysis_tabs",
          
          # Multi-method Consensus Analysis
          tabPanel(
            "Multi-method Consensus",
            
            fluidRow(
              column(
                width = 4,
                box(
                  title = "Consensus Configuration",
                  width = NULL,
                  status = "primary",
                  
                  selectizeInput(
                    "consensus_methods",
                    "Select Correlation Methods for Consensus:",
                    choices = NULL,
                    multiple = TRUE
                  ),
                  
                  uiOutput("consensus_weights_ui"),
                  
                  selectInput(
                    "consensus_group",
                    "Select Group:",
                    choices = NULL
                  ),
                  
                  numericInput(
                    "consensus_threshold",
                    "Consensus Threshold:",
                    value = 0.3,
                    min = 0,
                    max = 1,
                    step = 0.05
                  ),
                  
                  actionButton(
                    "run_consensus",
                    "Run Consensus Analysis",
                    icon = icon("play"),
                    class = "btn-success"
                  )
                )
              ),
              
              column(
                width = 8,
                box(
                  title = "Consensus Results",
                  width = NULL,
                  status = "info",
                  
                  plotlyOutput("consensus_network_plot", height = "400px"),
                  
                  hr(),
                  
                  tabsetPanel(
                    tabPanel(
                      "Consensus Matrix",
                      plotOutput("consensus_heatmap", height = "400px")
                    ),
                    tabPanel(
                      "Method Similarity",
                      plotOutput("method_similarity_plot", height = "400px")
                    ),
                    tabPanel(
                      "Metrics",
                      verbatimTextOutput("consensus_metrics")
                    )
                  )
                )
              )
            )
          ),
          
          # Network Stability Analysis
          tabPanel(
            "Network Stability",
            
            fluidRow(
              column(
                width = 4,
                box(
                  title = "Stability Configuration",
                  width = NULL,
                  status = "primary",
                  
                  selectInput(
                    "stability_method",
                    "Select Correlation Method:",
                    choices = NULL
                  ),
                  
                  selectInput(
                    "stability_group",
                    "Select Group:",
                    choices = NULL
                  ),
                  
                  numericInput(
                    "bootstrap_iterations",
                    "Bootstrap Iterations:",
                    value = 100,
                    min = 10,
                    max = 1000,
                    step = 10
                  ),
                  
                  numericInput(
                    "subsample_percentage",
                    "Subsample Percentage:",
                    value = 80,
                    min = 50,
                    max = 100,
                    step = 5
                  ),
                  
                  actionButton(
                    "run_stability",
                    "Run Stability Analysis",
                    icon = icon("play"),
                    class = "btn-success"
                  )
                )
              ),
              
              column(
                width = 8,
                box(
                  title = "Stability Results",
                  width = NULL,
                  status = "info",
                  
                  tabsetPanel(
                    tabPanel(
                      "Edge Stability",
                      plotOutput("edge_stability_plot", height = "400px")
                    ),
                    tabPanel(
                      "Node Stability",
                      plotOutput("node_stability_plot", height = "400px")
                    ),
                    tabPanel(
                      "Global Metrics CI",
                      plotOutput("global_stability_plot", height = "400px")
                    ),
                    tabPanel(
                      "Stability Metrics",
                      verbatimTextOutput("stability_metrics")
                    )
                  )
                )
              )
            )
          ),
          
          # Information Theory Analysis
          tabPanel(
            "Information Theory",
            
            fluidRow(
              column(
                width = 4,
                box(
                  title = "Information Theory Configuration",
                  width = NULL,
                  status = "primary",
                  
                  selectInput(
                    "info_theory_method",
                    "Select Correlation Method:",
                    choices = NULL
                  ),
                  
                  selectInput(
                    "info_theory_group",
                    "Select Group:",
                    choices = NULL
                  ),
                  
                  numericInput(
                    "info_bins",
                    "Number of Bins for Discretization:",
                    value = 6,
                    min = 2,
                    max = 20,
                    step = 1
                  ),
                  
                  selectInput(
                    "info_measure",
                    "Information Measure:",
                    choices = c(
                      "Mutual Information" = "mutual_information",
                      "Transfer Entropy" = "transfer_entropy",
                      "Multivariate Information" = "multivariate_info"
                    ),
                    selected = "mutual_information"
                  ),
                  
                  actionButton(
                    "run_info_theory",
                    "Run Information Theory Analysis",
                    icon = icon("play"),
                    class = "btn-success"
                  )
                )
              ),
              
              column(
                width = 8,
                box(
                  title = "Information Theory Results",
                  width = NULL,
                  status = "info",
                  
                  tabsetPanel(
                    tabPanel(
                      "Information Network",
                      plotlyOutput("info_network_plot", height = "400px")
                    ),
                    tabPanel(
                      "Information Patterns",
                      plotOutput("info_pattern_plot", height = "400px"),
                      verbatimTextOutput("info_pattern_summary")
                    ),
                    tabPanel(
                      "Region Metrics",
                      plotOutput("region_info_plot", height = "400px"),
                      DT::dataTableOutput("region_info_table")
                    ),
                    tabPanel(
                      "Information Matrices",
                      selectInput(
                        "info_matrix_type",
                        "Matrix Type:",
                        choices = c(
                          "Mutual Information" = "mutual_information",
                          "Transfer Entropy" = "transfer_entropy",
                          "Synergy" = "synergy",
                          "Redundancy" = "redundancy",
                          "Unique Information" = "unique_information"
                        ),
                        selected = "mutual_information"
                      ),
                      plotlyOutput("info_matrix_heatmap", height = "500px"),
                      downloadButton(
                        "download_info_matrix",
                        "Download Matrix"
                      )
                    )
                  )
                )
              )
            )
          ),
          
          # Brain-Behavior Prediction
          tabPanel(
            "Brain-Behavior Prediction",
            
            fluidRow(
              column(
                width = 4,
                box(
                  title = "Prediction Configuration",
                  width = NULL,
                  status = "primary",
                  
                  fileInput(
                    "behavior_data",
                    "Upload Behavioral Data (CSV):",
                    accept = c("text/csv", "text/comma-separated-values", ".csv")
                  ),
                  
                  selectInput(
                    "prediction_method",
                    "Correlation Method:",
                    choices = NULL,
                    selected = NULL
                  ),
                  
                  selectInput(
                    "subject_id_column_pred",
                    "Subject ID Column:",
                    choices = NULL
                  ),
                  
                  selectInput(
                    "behavior_column",
                    "Behavior Variable to Predict:",
                    choices = NULL
                  ),
                  
                  selectInput(
                    "prediction_model",
                    "Prediction Model:",
                    choices = c(
                      "Ridge Regression" = "ridge",
                      "Random Forest" = "rf",
                      "Support Vector Machine" = "svm",
                      "Neural Network" = "nnet"
                    ),
                    selected = "ridge"
                  ),
                  
                  numericInput(
                    "cv_folds",
                    "Cross-Validation Folds:",
                    value = 5,
                    min = 3,
                    max = 10,
                    step = 1
                  ),
                  
                  actionButton(
                    "run_prediction",
                    "Run Prediction",
                    icon = icon("play"),
                    class = "btn-success"
                  )
                )
              ),
              
              column(
                width = 8,
                box(
                  title = "Prediction Results",
                  width = NULL,
                  status = "info",
                  
                  tabsetPanel(
                    tabPanel(
                      "Prediction Performance",
                      plotOutput("prediction_performance_plot", height = "400px"),
                      verbatimTextOutput("prediction_metrics")
                    ),
                    tabPanel(
                      "Feature Importance",
                      plotOutput("feature_importance_plot", height = "400px"),
                      DT::dataTableOutput("feature_importance_table")
                    ),
                    tabPanel(
                      "Scatter Plot",
                      plotOutput("prediction_scatter_plot", height = "400px")
                    ),
                    tabPanel(
                      "Download Results",
                      downloadButton(
                        "download_prediction_results",
                        "Download Full Results"
                      ),
                      hr(),
                      verbatimTextOutput("prediction_summary")
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # Action buttons
    fluidRow(
      column(
        width = 12,
        div(
          style = "text-align: center; margin-top: 20px; margin-bottom: 20px;",
          actionButton(
            "proceed_to_results",
            "Proceed to Results & Export",
            icon = icon("arrow-right"),
            class = "btn-primary btn-lg"
          )
        )
      )
    )
  )
)
