#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/network/ui.R - Network analysis UI
#########################################################

fluidPage(
  fluidRow(
    column(
      width = 12,
      box(
        title = "Network Analysis",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        p("Calculate and visualize network metrics from correlation matrices."),
        p("Explore various graph theory metrics and community structure.")
      )
    )
  ),
  
  # Initial loading message
  uiOutput("network_initial_message"),
  
  # Main network analysis UI - only shown when correlation is complete
  conditionalPanel(
    condition = "output.correlation_complete",
    
    fluidRow(
      # Left column - Network selection and metrics
      column(
        width = 4,
        box(
          title = "Select Network",
          width = NULL,
          status = "primary",
          
          selectInput(
            "network_matrix",
            "Select Correlation Matrix:",
            choices = c("Loading matrices..."),
            selected = NULL
          ),
          
          hr(),
          
          checkboxGroupInput(
            "global_metrics",
            "Global Network Metrics:",
            choices = NETWORK_METRICS$Global,
            selected = c("density", "clustering", "modularity")
          ),
          
          checkboxGroupInput(
            "node_metrics",
            "Node-level Metrics:",
            choices = NETWORK_METRICS$`Node-level`,
            selected = c("degree", "betweenness", "eigenvector")
          )
        ),
        
        box(
          title = "Community Detection",
          width = NULL,
          status = "primary",
          
          selectInput(
            "community_method",
            "Community Detection Method:",
            choices = c(
              "Louvain" = "louvain",
              "Walktrap" = "walktrap",
              "Fast Greedy" = "fast_greedy",
              "Edge Betweenness" = "edge_betweenness",
              "Leading Eigenvector" = "leading_eigen",
              "Spinglass" = "spinglass"
            ),
            selected = "louvain"
          ),
          
          sliderInput(
            "min_community_size",
            "Minimum Community Size:",
            min = 1,
            max = 10,
            value = 3,
            step = 1
          )
        )
      ),
      
      # Middle column - Network stability and structure-function coupling
      column(
        width = 4,
        box(
          title = "Network Stability",
          width = NULL,
          status = "primary",
          
          checkboxInput(
            "run_bootstrap",
            "Run Bootstrap Analysis",
            value = FALSE
          ),
          
          conditionalPanel(
            condition = "input.run_bootstrap == true",
            sliderInput(
              "bootstrap_iterations",
              "Number of Bootstrap Iterations:",
              min = 100,
              max = 1000,
              value = 500,
              step = 100
            ),
            
            numericInput(
              "bootstrap_sample_percent",
              "Bootstrap Sample Size (% of original):",
              value = 80,
              min = 50,
              max = 100,
              step = 5
            ),
            
            checkboxInput(
              "calculate_stability_coefficient",
              "Calculate Correlation Stability Coefficient",
              value = TRUE
            )
          )
        ),
        
        box(
          title = "Structure-Function Coupling",
          width = NULL,
          status = "primary",
          
          checkboxInput(
            "run_structure_function",
            "Run Structure-Function Analysis",
            value = FALSE
          ),
          
          conditionalPanel(
            condition = "input.run_structure_function == true",
            fileInput(
              "structural_connectivity",
              "Upload Structural Connectivity Matrix (CSV):",
              accept = c("text/csv", "text/comma-separated-values", ".csv")
            ),
            
            selectInput(
              "structure_function_model",
              "Coupling Model:",
              choices = c(
                "Direct Matching" = "direct",
                "Euclidean Distance" = "euclidean",
                "Communicability" = "communicability",
                "Shortest Path" = "shortest_path",
                "Diffusion" = "diffusion",
                "Multiple Predictors" = "multiple"
              ),
              selected = "direct"
            ),
            
            checkboxInput(
              "include_indirect_paths",
              "Include Indirect Paths",
              value = TRUE
            )
          )
        )
      ),
      
      # Right column - Hub mapping and rich club
      column(
        width = 4,
        box(
          title = "Hub Mapping",
          width = NULL,
          status = "primary",
          
          selectInput(
            "hub_metric",
            "Primary Hub Metric:",
            choices = NETWORK_METRICS$`Node-level`,
            selected = "degree"
          ),
          
          sliderInput(
            "hub_threshold",
            "Hub Threshold (percentile):",
            min = 60,
            max = 95,
            value = 80,
            step = 5
          ),
          
          checkboxInput(
            "use_multivariate_hub_criteria",
            "Use Multivariate Hub Criteria",
            value = FALSE
          ),
          
          conditionalPanel(
            condition = "input.use_multivariate_hub_criteria == true",
            checkboxGroupInput(
              "secondary_hub_metrics",
              "Secondary Hub Metrics:",
              choices = NETWORK_METRICS$`Node-level`,
              selected = c("betweenness", "participation")
            )
          )
        ),
        
        box(
          title = "Rich Club Analysis",
          width = NULL,
          status = "primary",
          
          checkboxInput(
            "run_rich_club",
            "Run Rich Club Analysis",
            value = FALSE
          ),
          
          conditionalPanel(
            condition = "input.run_rich_club == true",
            sliderInput(
              "rich_club_levels",
              "Number of k-levels:",
              min = 5,
              max = 20,
              value = 10,
              step = 1
            ),
            
            numericInput(
              "randomization_iterations",
              "Randomization Iterations:",
              value = 100,
              min = 10,
              max = 1000,
              step = 10
            )
          )
        )
      )
    ),
    
    # Results preview
    fluidRow(
      column(
        width = 12,
        tabBox(
          title = "Network Analysis Results",
          width = NULL,
          height = "600px",
          
          tabPanel(
            "Network Visualization",
            plotlyOutput("network_graph", height = "550px")
          ),
          
          tabPanel(
            "Global Metrics",
            plotOutput("global_metrics_plot", height = "250px"),
            DT::dataTableOutput("global_metrics_table")
          ),
          
          tabPanel(
            "Node Metrics",
            plotOutput("node_metrics_plot", height = "250px"),
            DT::dataTableOutput("node_metrics_table")
          ),
          
          tabPanel(
            "Community Structure",
            plotOutput("community_plot", height = "550px")
          ),
          
          tabPanel(
            "Hub Analysis",
            plotOutput("hub_plot", height = "550px")
          ),
          
          tabPanel(
            "Network Stability",
            plotOutput("stability_plot", height = "250px"),
            verbatimTextOutput("stability_results")
          ),
          
          tabPanel(
            "Structure-Function",
            plotOutput("structure_function_plot", height = "250px"),
            verbatimTextOutput("structure_function_results")
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
            "run_network_analysis",
            "Run Network Analysis",
            icon = icon("play"),
            class = "btn-success btn-lg"
          ),
          actionButton(
            "proceed_to_visualization",
            "Proceed to Visualization",
            icon = icon("arrow-right"),
            class = "btn-primary btn-lg",
            style = "margin-left: 20px;"
          )
        )
      )
    )
  )
)
