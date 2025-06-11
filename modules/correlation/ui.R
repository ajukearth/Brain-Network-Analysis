#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/correlation/ui.R - Correlation methods UI
#########################################################

fluidPage(
  fluidRow(
    column(
      width = 12,
      box(
        title = "Correlation Methods",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        p("Select and configure correlation methods to generate functional connectivity matrices."),
        p("Different methods capture different aspects of brain connectivity. You can select multiple methods for comparison.")
      )
    )
  ),
  
  # Initial loading message
  uiOutput("correlation_initial_message"),
  
  # Main correlation methods UI - only shown when data is ready
  conditionalPanel(
    condition = "output.preprocessing_complete",
    
    fluidRow(
      # Left column - Method selection
      column(
        width = 4,
        box(
          title = "Select Correlation Methods",
          width = NULL,
          status = "primary",
          
          selectInput(
            "primary_method",
            "Primary Correlation Method:",
            choices = unlist(CORRELATION_METHODS, use.names = TRUE),
            selected = "pearson"
          ),
          
          checkboxGroupInput(
            "additional_methods",
            "Additional Methods (Optional):",
            choices = unlist(CORRELATION_METHODS, use.names = TRUE),
            selected = NULL
          ),
          
          checkboxInput(
            "use_ensemble",
            "Generate ensemble matrix from selected methods",
            value = FALSE
          ),
          
          conditionalPanel(
            condition = "input.use_ensemble == true",
            selectInput(
              "ensemble_method", 
              "Ensemble Method:",
              choices = c(
                "Average" = "average",
                "Weighted Average" = "weighted",
                "Rank Aggregation" = "rank",
                "Consensus Clustering" = "consensus"
              ),
              selected = "average"
            )
          )
        )
      ),
      
      # Middle column - Method configuration
      column(
        width = 4,
        uiOutput("correlation_method_settings")
      ),
      
      # Right column - Threshold settings
      column(
        width = 4,
        box(
          title = "Thresholding",
          width = NULL,
          status = "primary",
          
          selectInput(
            "threshold_type",
            "Threshold Type:",
            choices = c(
              "None" = "none",
              "Absolute Value" = "absolute",
              "Proportional" = "proportional",
              "Statistical Significance" = "significance"
            ),
            selected = "absolute"
          ),
          
          conditionalPanel(
            condition = "input.threshold_type == 'absolute'",
            sliderInput(
              "absolute_threshold",
              "Absolute Threshold Value:",
              min = 0,
              max = 1,
              value = DEFAULT_THRESHOLD,
              step = 0.05
            )
          ),
          
          conditionalPanel(
            condition = "input.threshold_type == 'proportional'",
            sliderInput(
              "proportional_threshold",
              "Connection Density (%):",
              min = 1,
              max = 100,
              value = 20,
              step = 1
            )
          ),
          
          conditionalPanel(
            condition = "input.threshold_type == 'significance'",
            sliderInput(
              "significance_threshold",
              "P-value Threshold:",
              min = 0.001,
              max = 0.1,
              value = 0.05,
              step = 0.001
            ),
            checkboxInput(
              "fdr_correction",
              "Apply FDR Correction",
              value = TRUE
            )
          ),
          
          checkboxInput(
            "use_absolute_values",
            "Use Absolute Correlation Values",
            value = FALSE
          ),
          
          checkboxInput(
            "include_negative",
            "Include Negative Correlations",
            value = TRUE
          )
        )
      )
    ),
    
    # Correlation matrix preview
    fluidRow(
      column(
        width = 12,
        box(
          title = "Correlation Matrix Preview",
          width = NULL,
          status = "primary",
          
          fluidRow(
            column(
              width = 9,
              plotlyOutput("correlation_heatmap", height = "500px")
            ),
            column(
              width = 3,
              selectInput(
                "preview_method",
                "Preview Method:",
                choices = c("Primary Method"),
                selected = "Primary Method"
              ),
              
              sliderInput(
                "heatmap_text_size",
                "Text Size:",
                min = 6,
                max = 14,
                value = 8,
                step = 1
              ),
              
              checkboxInput(
                "show_dendrogram",
                "Show Dendrogram",
                value = TRUE
              ),
              
              downloadButton(
                "download_correlation_matrix",
                "Download Matrix"
              ),
              
              hr(),
              
              verbatimTextOutput("correlation_summary")
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
            "run_correlation",
            "Generate Correlation Matrices",
            icon = icon("play"),
            class = "btn-success btn-lg"
          ),
          actionButton(
            "proceed_to_network",
            "Proceed to Network Analysis",
            icon = icon("arrow-right"),
            class = "btn-primary btn-lg",
            style = "margin-left: 20px;"
          )
        )
      )
    )
  )
)