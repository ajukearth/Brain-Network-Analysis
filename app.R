#########################################################
# Enhanced Brain Network Analysis Shiny App
# app.R - Main application file
#########################################################

# Source global script for package loading and constants
source("global.R")

# Define UI
ui <- shinydashboardPlus::dashboardPage(
  # Dashboard header
  shinydashboardPlus::dashboardHeader(
    title = span(
      img(src = "logo.png", height = 30),
      "Brain Network Analysis"
    ),
    titleWidth = 300,
    controlbarIcon = icon("cogs"),
    userOutput("userBox"),
    #rightUi = tagList(
    #  actionButton("about_btn", "About", icon = icon("info-circle")),
    #  actionButton("help_btn", "Help", icon = icon("question-circle"))
    #)
  ),
  
  # Dashboard sidebar
  shinydashboardPlus::dashboardSidebar(
    width = 300,
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("1. Data Import", tabName = "data_import", icon = icon("upload")),
      shinydashboard::menuItem("2. Preprocessing", tabName = "preprocessing", icon = icon("broom")),
      shinydashboard::menuItem("3. Correlation Methods", tabName = "correlation", icon = icon("project-diagram")),
      shinydashboard::menuItem("4. Network Analysis", tabName = "network", icon = icon("network-wired")),
      shinydashboard::menuItem("5. Visualization", tabName = "visualization", icon = icon("chart-bar")),
      shinydashboard::menuItem("6. Advanced Analysis", tabName = "advanced", icon = icon("brain")),
      shinydashboard::menuItem("7. Results", tabName = "results", icon = icon("file-export"))
    ),
    
    shiny::hr(),
    
    # Progress indicator
    shiny::uiOutput("progress_indicator"),
    
    shiny::hr(),
    
    # App information footer
    shiny::div(
      style = "padding: 15px;",
      shiny::p("Brain Network Analysis App", style = "font-weight: bold;"),
      shiny::p(paste("Version", APP_VERSION)),
      shiny::p("© 2025", style = "font-size: 90%;")
    )
  ),
  
  # Dashboard body
  shinydashboardPlus::dashboardBody(
    # Use shinyjs
    shinyjs::useShinyjs(),
    
    # Include custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    # Tab content
    shinydashboard::tabItems(
      # Data Import tab
      shinydashboard::tabItem(
        tabName = "data_import",
        source("modules/import/ui.R", local = TRUE)$value
      ),
      
      # Preprocessing tab
      shinydashboard::tabItem(
        tabName = "preprocessing",
        source("modules/preprocess/ui.R", local = TRUE)$value
      ),
      
      # Correlation Methods tab
      shinydashboard::tabItem(
        tabName = "correlation",
        source("modules/correlation/ui.R", local = TRUE)$value
      ),
      
      # Network Analysis tab
      shinydashboard::tabItem(
        tabName = "network",
        source("modules/network/ui.R", local = TRUE)$value
      ),
      
      # Visualization tab
      shinydashboard::tabItem(
        tabName = "visualization",
        source("modules/visualization/ui.R", local = TRUE)$value
      ),
      
      # Advanced Analysis tab
      shinydashboard::tabItem(
        tabName = "advanced",
        source("modules/advanced/ui.R", local = TRUE)$value
      ),
      
      # Results tab
      shinydashboard::tabItem(
        tabName = "results",
        source("modules/results/ui.R", local = TRUE)$value
      )
    )
  ),
  
  # Dashboard control bar (additional settings)
  shinydashboardPlus::dashboardControlbar(
    id = "controlbar",
    width = 300,
    shinydashboardPlus::controlbarMenu(
      id = "controlbarMenu",
      type = "pills",
      shinydashboardPlus::controlbarItem(
        "Settings",
        h4("Global Settings"),
        sliderInput("global_precision", "Numeric Precision", 
                    min = 2, max = 8, value = 4, step = 1),
        checkboxInput("use_parallel", "Use Parallel Computing", value = FALSE),
        numericInput("num_cores", "Number of Cores", value = 2, min = 1, max = 16),
        hr(),
        h4("Visual Settings"),
        selectInput("color_theme", "Color Theme", 
                    choices = c("Default", "Viridis", "Colorblind Friendly", "Custom")),
        conditionalPanel(
          condition = "input.color_theme == 'Custom'",
          colourInput("positive_color", "Positive Correlation", value = "#FF5555"),
          colourInput("negative_color", "Negative Correlation", value = "#5555FF")
        ),
        checkboxInput("dark_mode", "Dark Mode", value = FALSE)
      ),
      shinydashboardPlus::controlbarItem(
        "Info",
        h4("System Information"),
        verbatimTextOutput("system_info"),
        h4("Session Information"),
        verbatimTextOutput("session_info"),
        h4("Memory Usage"),
        verbatimTextOutput("memory_usage")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initialize reactive values for workflow tracking
  workflow <- shiny::reactiveValues(
    data_import_complete = FALSE,
    preprocessing_complete = FALSE,
    correlation_complete = FALSE,
    network_complete = FALSE,
    visualization_complete = FALSE,
    advanced_complete = FALSE,
    results_complete = FALSE
  )
  
  # Initialize reactive values for data storage
  data_store <- shiny::reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    correlation_matrices = list(),
    network_metrics = list(),
    visualization_objects = list(),
    advanced_results = list(),
    export_items = list()
  )
  
  # Source module server logic
  source("modules/import/server.R", local = TRUE)
  source("modules/preprocess/server.R", local = TRUE)
  source("modules/correlation/server.R", local = TRUE)
  source("modules/network/server.R", local = TRUE)
  source("modules/visualization/server.R", local = TRUE)
  source("modules/advanced/server.R", local = TRUE)
  source("modules/results/server.R", local = TRUE)
  
  # Progress indicator in sidebar
  output$progress_indicator <- shiny::renderUI({
    shiny::div(
      class = "progress-indicator",
      
      shiny::div(
        class = paste(
          "progress-item",
          if (input$tabs == "data_import") "progress-active" 
          else if (workflow$data_import_complete) "progress-complete" 
          else "progress-pending"
        ),
        shiny::icon(if (workflow$data_import_complete) "check" else "circle"),
        "1. Data Import"
      ),
      
      shiny::div(
        class = paste(
          "progress-item",
          if (input$tabs == "preprocessing") "progress-active" 
          else if (workflow$preprocessing_complete) "progress-complete" 
          else "progress-pending"
        ),
        shiny::icon(if (workflow$preprocessing_complete) "check" else "circle"),
        "2. Preprocessing"
      ),
      
      shiny::div(
        class = paste(
          "progress-item",
          if (input$tabs == "correlation") "progress-active" 
          else if (workflow$correlation_complete) "progress-complete" 
          else "progress-pending"
        ),
        shiny::icon(if (workflow$correlation_complete) "check" else "circle"),
        "3. Correlation Methods"
      ),
      
      shiny::div(
        class = paste(
          "progress-item",
          if (input$tabs == "network") "progress-active" 
          else if (workflow$network_complete) "progress-complete" 
          else "progress-pending"
        ),
        shiny::icon(if (workflow$network_complete) "check" else "circle"),
        "4. Network Analysis"
      ),
      
      shiny::div(
        class = paste(
          "progress-item",
          if (input$tabs == "visualization") "progress-active" 
          else if (workflow$visualization_complete) "progress-complete" 
          else "progress-pending"
        ),
        shiny::icon(if (workflow$visualization_complete) "check" else "circle"),
        "5. Visualization"
      ),
      
      shiny::div(
        class = paste(
          "progress-item",
          if (input$tabs == "advanced") "progress-active" 
          else if (workflow$advanced_complete) "progress-complete" 
          else "progress-pending"
        ),
        shiny::icon(if (workflow$advanced_complete) "check" else "circle"),
        "6. Advanced Analysis"
      ),
      
      shiny::div(
        class = paste(
          "progress-item",
          if (input$tabs == "results") "progress-active" 
          else if (workflow$results_complete) "progress-complete" 
          else "progress-pending"
        ),
        shiny::icon(if (workflow$results_complete) "check" else "circle"),
        "7. Results"
      )
    )
  })
  
  # System information
  output$system_info <- renderPrint({
    list(
      R_version = R.version.string,
      OS = Sys.info()["sysname"],
      Cores = parallel::detectCores()
    )
  })
  
  # Session information
  output$session_info <- renderPrint({
    list(
      Session_ID = session$token,
      Client = session$clientData$url_hostname,
      Browser = session$clientData$browser
    )
  })
  
  # Memory usage
  output$memory_usage <- renderPrint({
    gc_info <- gc()
    list(
      Used_Memory_MB = sum(gc_info[,2]),
      Available_Memory_MB = memory.limit()
    )
  })
  
  # User info box
  output$userBox <- shinydashboardPlus::renderUser({
    shinydashboardPlus::dashboardUser(
      name = "User",
      subtitle = "Researcher",
      image = "https://image.flaticon.com/icons/svg/236/236832.svg"
    )
  })
  
  # About modal
  observeEvent(input$about_btn, {
    showModal(modalDialog(
      title = "About Brain Network Analysis App",
      includeMarkdown("www/about.md"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Help modal
  observeEvent(input$help_btn, {
    showModal(modalDialog(
      title = "Help",
      h4("Quick Start Guide"),
      p("This application allows you to analyze functional connectivity networks in brain data through a step-by-step workflow:"),
      tags$ol(
        tags$li("Import your data in CSV or Excel format"),
        tags$li("Preprocess data to handle missing values and quality checks"),
        tags$li("Select correlation method(s) to generate connectivity matrices"),
        tags$li("Perform network analysis with graph theory metrics"),
        tags$li("Visualize results with interactive plots"),
        tags$li("Run advanced analyses like bootstrapping or consensus methods"),
        tags$li("Export results and reports")
      ),
      p("For detailed instructions, please refer to the User Guide in the documentation folder."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Handle manual tab navigation
  observeEvent(input$tabs, {
    # When user manually navigates, check if they're going to a tab that requires previous steps
    current_tab <- input$tabs
    
    # Define required workflows for each tab
    tab_requirements <- list(
      "data_import" = NULL,  # No requirements for first tab
      "preprocessing" = "data_import_complete",
      "correlation" = "preprocessing_complete",
      "network" = "correlation_complete",
      "visualization" = "network_complete",
      "advanced" = "visualization_complete",
      "results" = "network_complete"  # Results only requires network, not advanced
    )
    
    # Check if current tab has requirements
    if (!is.null(tab_requirements[[current_tab]])) {
      required_workflow <- tab_requirements[[current_tab]]
      
      # If requirement is not met, show warning and go back to appropriate tab
      if (!is.null(required_workflow) && !workflow[[required_workflow]]) {
        # Determine which tab to go back to
        go_back_to <- switch(current_tab,
                             "preprocessing" = "data_import",
                             "correlation" = "preprocessing",
                             "network" = "correlation",
                             "visualization" = "network",
                             "advanced" = "visualization",
                             "results" = "network"
        )
        
        # Show warning
        shiny::showNotification(
          paste("Please complete the", gsub("_", " ", go_back_to), "step first."),
          type = "warning",
          duration = 5
        )
        
        # Navigate back to appropriate tab
        shinydashboard::updateTabItems(session, "tabs", go_back_to)
      }
    }
  })
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
