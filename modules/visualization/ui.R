#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/visualization/ui.R - Visualization UI
#########################################################

# Define visualization UI
fluidPage(
  # Initial message based on network analysis state
  uiOutput("visualization_initial_message"),
  
  # Main visualization UI - only shown when network analysis is complete
  conditionalPanel(
    condition = "output.network_complete",
    
    fluidRow(
      column(
        width = 12,
        h3("Network Visualization"),
        p("Explore visual representations of your brain network analysis results."),
        hr()
      )
    ),
    
    fluidRow(
      # Network Graph Panel
      column(
        width = 6,
        box(
          title = "Network Graph",
          width = NULL,
          status = "primary",
          
          selectInput(
            "plot_group",
            "Select Group:",
            choices = NULL,
            selected = NULL
          ),
          
          selectInput(
            "layout_type",
            "Layout Algorithm:",
            choices = c(
              "Fruchterman-Reingold" = "fr",
              "Kamada-Kawai" = "kk",
              "Circle" = "circle",
              "Grid" = "grid",
              "Sphere (3D)" = "sphere"
            ),
            selected = "fr"
          ),
          
          selectInput(
            "color_by",
            "Color Nodes By:",
            choices = c(
              "Brain Area" = "brain_area",
              "Degree" = "degree",
              "Betweenness" = "betweenness",
              "Closeness" = "closeness",
              "Clustering" = "clustering"
            ),
            selected = "brain_area"
          ),
          
          selectInput(
            "size_by",
            "Size Nodes By:",
            choices = c(
              "Uniform Size" = "none",
              "Degree" = "degree",
              "Betweenness" = "betweenness",
              "Closeness" = "closeness",
              "Clustering" = "clustering"
            ),
            selected = "none"
          ),
          
          checkboxInput(
            "show_labels",
            "Show Node Labels",
            value = FALSE
          ),
          
          checkboxInput(
            "use_3d",
            "Use 3D Visualization",
            value = FALSE
          ),
          
          hr(),
          
          plotlyOutput("network_viz_plot", height = "500px"),
          
          downloadButton(
            "download_network_plot",
            "Download Plot"
          )
        )
      ),
      
      # Heatmap and Metrics Panel
      column(
        width = 6,
        tabBox(
          title = "Analysis Results",
          width = NULL,
          
          tabPanel(
            "Correlation Heatmap",
            
            selectInput(
              "heatmap_group",
              "Select Group:",
              choices = NULL,
              selected = NULL
            ),
            
            checkboxInput(
              "show_dendrogram",
              "Show Dendrogram",
              value = TRUE
            ),
            
            plotOutput("correlation_heatmap", height = "500px"),
            
            downloadButton(
              "download_heatmap",
              "Download Heatmap"
            )
          ),
          
          tabPanel(
            "Global Metrics",
            
            plotOutput("global_metrics_viz", height = "250px"),
            
            DT::dataTableOutput("global_metrics_table"),
            
            downloadButton(
              "download_global_metrics",
              "Download Metrics"
            )
          ),
          
          tabPanel(
            "Node Metrics",
            
            selectInput(
              "node_metric_plot",
              "Select Metric to Visualize:",
              choices = c(
                "Degree" = "degree",
                "Betweenness Centrality" = "betweenness",
                "Closeness Centrality" = "closeness",
                "Clustering Coefficient" = "clustering",
                "Eigenvector Centrality" = "eigenvector"
              ),
              selected = "degree"
            ),
            
            plotOutput("node_metrics_plot", height = "250px"),
            
            DT::dataTableOutput("node_metrics_table"),
            
            downloadButton(
              "download_node_metrics",
              "Download Metrics"
            )
          ),
          
          tabPanel(
            "Brain Area Summary",
            
            plotOutput("brain_area_plot", height = "250px"),
            
            DT::dataTableOutput("brain_area_table"),
            
            downloadButton(
              "download_brain_area_metrics",
              "Download Summary"
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
            "regenerate_visualizations",
            "Regenerate Visualizations",
            icon = icon("sync"),
            class = "btn-success btn-lg"
          ),
          actionButton(
            "proceed_to_advanced",
            "Proceed to Advanced Analysis",
            icon = icon("arrow-right"),
            class = "btn-primary btn-lg",
            style = "margin-left: 20px;"
          )
        )
      )
    )
  )
)
