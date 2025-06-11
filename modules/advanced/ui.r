#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/advanced/ui.R - Advanced analysis UI
#########################################################

fluidPage(
  fluidRow(
    column(
      width = 12,
      box(
        title = "Advanced Analysis",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        p("Perform advanced network analyses, including fingerprinting and information flow analysis."),
        p("These techniques offer deeper insights into network dynamics and individual variability.")
      )
    )
  ),
  
  # Initial loading message
  uiOutput("advanced_initial_message"),
  
  # Main advanced analysis UI - only shown when visualization is complete
  conditionalPanel(
    condition = "output.visualization_complete",
    
    tabsetPanel(
      id = "advanced_tabs",
      
      # Multi-method Consensus Analysis
      tabPanel(
        "Multi-Method Consensus",
        
        fluidRow(
          column(
            width = 4,
            box(
              title = "Select Methods",
              width = NULL,
              status = "primary",
              
              selectizeInput(
                "consensus_methods",
                "Select Correlation Methods:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Select at least two methods"
                )
              ),
              
              selectInput(
                "consensus_approach",
                "Consensus Approach:",
                choices = c(
                  "Average" = "average",
                  "Weighted Average" = "weighted",
                  "Rank Aggregation" = "rank",
                  "Consensus Clustering" = "consensus"
                ),
                selected = "average"
              ),
              
              conditionalPanel(
                condition = "input.consensus_approach == 'weighted'",
                uiOutput("consensus_weights_ui")
              ),
              
              sliderInput(
                "consensus_threshold",
                "Connection Threshold:",
                min = 0,
                max = 1,
                value = 0.3,
                step = 0.05
              ),
              
              actionButton(
                "run_consensus",
                "Generate Consensus Matrix",
                icon = icon("play"),
                class = "btn-primary"
              )
            )
          ),
          
          column(
            width = 8,
            box(
              title = "Consensus Matrix",
              width = NULL,
              status = "primary",
              
              plotlyOutput("consensus_heatmap", height = "500px"),
              
              downloadButton(
                "download_consensus_matrix",
                "Download Matrix"
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            box(
              title = "Consensus Network Metrics",
              width = NULL,
              status = "primary",
              
              DT::dataTableOutput("consensus_metrics")
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
              title = "Bootstrap Configuration",
              width = NULL,
              status = "primary",
              
              selectInput(
                "stability_method",
                "Correlation Method:",
                choices = NULL,
                selected = NULL
              ),
              
              sliderInput(
                "bootstrap_iterations",
                "Number of Bootstrap Iterations:",
                min = 100,
                max = 1000,
                value = 500,
                step = 100
              ),
              
              sliderInput(
                "bootstrap_sample_percent",
                "Bootstrap Sample Size (%):",
                min = 50,
                max = 100,
                value = 80,
                step = 5
              ),
              
              checkboxInput(
                "calculate_cs_coefficient",
                "Calculate Correlation Stability Coefficient",
                value = TRUE
              ),
              
              checkboxGroupInput(
                "stability_metrics",
                "Metrics to Analyze:",
                choices = c(
                  "Density" = "density",
                  "Clustering" = "clustering",
                  "Modularity" = "modularity",
                  "Degree" = "degree",
                  "Betweenness" = "betweenness",
                  "Eigenvector" = "eigenvector"
                ),
                selected = c("density", "clustering", "degree")
              ),
              
              actionButton(
                "run_stability",
                "Run Stability Analysis",
                icon = icon("play"),
                class = "btn-primary"
              )
            )
          ),
          
          column(
            width = 8,
            box(
              title = "Stability Results",
              width = NULL,
              status = "primary",
              
              tabsetPanel(
                id = "stability_tabs",
                
                tabPanel(
                  "Global Metrics",
                  plotOutput("stability_global_plot", height = "350px"),
                  verbatimTextOutput("stability_global_summary")
                ),
                
                tabPanel(
                  "Node Metrics",
                  plotOutput("stability_node_plot", height = "350px"),
                  verbatimTextOutput("stability_node_summary")
                ),
                
                tabPanel(
                  "Correlation Stability",
                  plotOutput("cs_coefficient_plot", height = "350px"),
                  verbatimTextOutput("cs_coefficient_summary")
                ),
                
                tabPanel(
                  "Consensus Matrix",
                  plotlyOutput("bootstrap_consensus_heatmap", height = "400px")
                )
              )
            )
          )
        )
      ),
      
      # Individual Fingerprinting
      tabPanel(
        "Individual Fingerprinting",
        
        fluidRow(
          column(
            width = 4,
            box(
              title = "Fingerprinting Configuration",
              width = NULL,
              status = "primary",
              
              fileInput(
                "fingerprinting_data",
                "Upload Multiple Session Data (CSV):",
                accept = c("text/csv", "text/comma-separated-values", ".csv")
              ),
              
              selectInput(
                "fingerprinting_method",
                "Correlation Method:",
                choices = NULL,
                selected = NULL
              ),
              
              selectInput(
                "subject_id_column",
                "Subject ID Column:",
                choices = NULL
              ),
              
              selectizeInput(
                "fingerprinting_regions",
                "Select Brain Regions:",
                choices = NULL,
                multiple = TRUE
              ),
              
              actionButton(
                "run_fingerprinting",
                "Run Fingerprinting Analysis",
                icon = icon("play"),
                class = "btn-primary"
              )
            )
          ),
          
          column(
            width = 8,
            box(
              title = "Fingerprinting Results",
              width = NULL,
              status = "primary",
              
              tabsetPanel(
                id = "fingerprinting_tabs",
                
                tabPanel(
                  "Identifiability",
                  plotOutput("identifiability_plot", height = "400px"),
                  verbatimTextOutput("identifiability_summary")
                ),
                
                tabPanel(
                  "Edge Importance",
                  plotlyOutput("edge_importance_heatmap", height = "400px"),
                  hr(),
                  plotlyOutput("top_edges_plot", height = "300px")
                ),
                
                tabPanel(
                  "Subject Discriminability",
                  plotOutput("subject_discriminability_plot", height = "400px"),
                  DT::dataTableOutput("subject_discriminability_table")
                )
              )
            )
          )
        )
      ),
      
      # Information Flow Analysis
      tabPanel(
        "Information Flow",
        
        fluidRow(
          column(
            width = 4,
            box(
              title = "Information Flow Configuration",
              width = NULL,
              status = "primary",
              
              selectizeInput(
                "info_flow_regions",
                "Select Brain Regions:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Select regions (max 10 recommended)"
                )
              ),
              
              sliderInput(
                "time_lag",
                "Time Lag:",
                min = 1,
                max = 5,
                value = 1,
                step = 1
              ),
              
              sliderInput(
                "num_bins",
                "Number of Bins for Discretization:",
                min = 2,
                max = 10,
                value = 3,
                step = 1
              ),
              
              sliderInput(
                "info_flow_threshold",
                "Edge Threshold:",
                min = 0,
                max = 0.5,
                value = 0.1,
                step = 0.01
              ),
              
              actionButton(
                "run_info_flow",
                "Run Information Flow Analysis",
                icon = icon("play"),
                class = "btn-primary"
              )
            )
          ),
          
          column(
            width = 8,
            box(
              title = "Information Flow Results",
              width = NULL,
              status = "primary",
              
              tabsetPanel(
                id = "info_flow_tabs",
                
                tabPanel(
                  "Network Visualization",
                  selectInput(
                    "info_flow_viz_metric",
                    "Visualization Metric:",
                    choices = c(
                      "Mutual Information" = "mutual_information",
                      "Transfer Entropy" = "transfer_entropy",
                      "Synergy" = "synergy",
                      "Redundancy" = "redundancy",
                      "Unique Information" = "unique_information"
                    ),
                    selected = "transfer_entropy"
                  ),
                  plotlyOutput("info_flow_network", height = "500px")
                ),
                
                tabPanel(
                  "Pattern Distribution",
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
                "prediction_algorithm",
                "Prediction Algorithm:",
                choices = c(
                  "Kernel Ridge Regression" = "kernel_ridge",
                  "Linear Ridge Regression" = "linear_ridge",
                  "LASSO Regression" = "lasso",
                  "Random Forest" = "random_forest",
                  "Support Vector Regression" = "svr"
                ),
                selected = "kernel_ridge"
              ),
              
              sliderInput(
                "cv_folds",
                "Cross-Validation Folds:",
                min = 5,
                max = 20,
                value = 10,
                step = 1
              ),
              
              checkboxInput(
                "feature_selection",
                "Use Feature Selection",
                value = TRUE
              ),
              
              actionButton(
                "run_prediction",
                "Run Prediction Analysis",
                icon = icon("play"),
                class = "btn-primary"
              )
            )
          ),
          
          column(
            width = 8,
            box(
              title = "Prediction Results",
              width = NULL,
              status = "primary",
              
              tabsetPanel(
                id = "prediction_tabs",
                
                tabPanel(
                  "Performance",
                  plotOutput("prediction_performance_plot", height = "400px"),
                  verbatimTextOutput("prediction_metrics")
                ),
                
                tabPanel(
                  "Feature Importance",
                  plotlyOutput("feature_importance_plot", height = "400px"),
                  DT::dataTableOutput("top_features_table")
                ),
                
                tabPanel(
                  "Permutation Testing",
                  plotOutput("permutation_plot", height = "400px"),
                  verbatimTextOutput("permutation_results")
                )
              )
            )
          )
        )
      )
    )
  )
)
