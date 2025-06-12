#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/import/ui.R - Import module UI components
#########################################################

# Import module UI
tagList(
  fluidRow(
    column(
      width = 12,
      box(
        title = "Data Import",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        
        p("Upload your connectivity data file to begin the analysis."),
        
        fluidRow(
          column(
            width = 6,
            fileInput(
              "import_file", 
              "Upload CSV or Excel File:",
              accept = c(
                "text/csv",
                "application/vnd.ms-excel",
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
              ),
              buttonLabel = "Browse...",
              placeholder = "No file selected"
            )
          ),
          
          column(
            width = 6,
            conditionalPanel(
              condition = "output.is_excel_file",
              selectInput("import_sheet", "Select Excel Sheet:", choices = NULL)
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(
              style = "text-align: right;",
              downloadButton("download_template", "Download Template", icon = icon("download"))
            )
          )
        )
      )
    )
  ),
  
  conditionalPanel(
    condition = "output.data_loaded",
    fluidRow(
      column(
        width = 12,
        box(
          title = "Data Configuration",
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          
          fluidRow(
            column(
              width = 4,
              selectInput(
                "import_id_column",
                "Subject/ID Column:",
                choices = c("Select column" = "")
              )
            ),
            
            column(
              width = 4,
              selectizeInput(
                "import_group_columns",
                "Group Column(s):",
                choices = c("Select column(s)" = ""),
                multiple = TRUE,
                options = list(plugins = list("remove_button"))
              )
            ),
            
            column(
              width = 4,
              selectizeInput(
                "import_behavior_columns",
                "Behavioral Measure(s) (Optional):",
                choices = c("Select column(s)" = ""),
                multiple = TRUE,
                options = list(plugins = list("remove_button"))
              )
            )
          ),
          
          fluidRow(
            column(
              width = 6,
              checkboxInput("import_combine_groups", "Create Combined Group Column", value = FALSE)
            ),
            
            column(
              width = 6,
              checkboxInput("import_transpose", "Transpose Data (Columns as Subjects)", value = FALSE)
            )
          ),
          
          hr(),
          
          fluidRow(
            column(
              width = 12,
              div(
                style = "text-align: center;",
                actionButton(
                  "import_configure_btn", 
                  "Configure Data",
                  icon = icon("cogs"),
                  class = "btn-primary"
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
        tabBox(
          title = "Data Preview",
          width = NULL,
          id = "import_preview_tabs",
          
          tabPanel(
            "Raw Data",
            DT::DTOutput("import_data_preview")
          ),
          
          tabPanel(
            "Column Summary",
            verbatimTextOutput("import_column_summary")
          ),
          
          tabPanel(
            "Data Structure",
            verbatimTextOutput("import_structure")
          )
        )
      )
    )
  ),
  
  # Data quality summary when configuration is complete
  conditionalPanel(
    condition = "output.config_complete",
    fluidRow(
      column(
        width = 12,
        box(
          title = "Data Quality Summary",
          width = NULL,
          status = "success",
          solidHeader = TRUE,
          
          uiOutput("import_quality_report"),
          
          conditionalPanel(
            condition = "output.has_missing_data",
            fluidRow(
              column(
                width = 12,
                div(
                  class = "alert alert-warning",
                  icon("exclamation-triangle"),
                  "Missing data detected. You can handle missing values in the Preprocessing step."
                )
              )
            )
          ),
          
          fluidRow(
            column(
              width = 12,
              div(
                style = "text-align: center; margin-top: 20px;",
                actionButton(
                  "import_proceed_btn",
                  "Proceed to Preprocessing",
                  icon = icon("arrow-right"),
                  class = "btn-lg btn-success"
                )
              )
            )
          )
        )
      )
    )
  )
)
