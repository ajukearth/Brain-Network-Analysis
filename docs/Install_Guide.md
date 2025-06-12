# Enhanced Brain Network Analysis - Installation Guide

This guide provides detailed instructions for installing and setting up the Enhanced Brain Network Analysis application in different environments.

## System Requirements

- **R version**: 4.0.0 or higher (4.2.0+ recommended)
- **Operating System**: Windows, macOS, or Linux
- **RAM**: Minimum 4GB, 8GB+ recommended for larger datasets
- **Dependencies**: Internet connection for package installation

## Installation Methods

### Method 1: Using RStudio (Recommended for Most Users)

1. **Install R and RStudio**:
   - Download and install R from [CRAN](https://cran.r-project.org/)
   - Download and install RStudio from [RStudio Downloads](https://www.rstudio.com/products/rstudio/download/)

2. **Clone or Download the Repository**:
   - Option 1: Use Git
     ```bash
     git clone https://github.com/yourusername/brain-network-analysis.git
     ```
   - Option 2: Download ZIP file from GitHub and extract

3. **Open the Project in RStudio**:
   - In RStudio, select File → Open Project
   - Navigate to the brain-network-analysis directory and select the .Rproj file

4. **Install Required Packages**:
   - Open the R console in RStudio and run:
     ```R
     # Run the setup script
     source("setup.R")
     ```
   - Alternatively, install packages manually:
     ```R
     install.packages(c(
       "shiny", "shinydashboard", "shinydashboardPlus", "shinyjs", "shinyWidgets",
       "DT", "dplyr", "ggplot2", "igraph", "reshape2", "tidyr", "readr", 
       "openxlsx", "colourpicker", "RColorBrewer", "mice", "naniar", "ggraph", 
       "scales", "Cairo", "viridis", "plotly", "htmlwidgets", "heatmaply",
       "visNetwork", "markdown", "knitr", "rmarkdown", "zip"
     ))
     ```

5. **Run the Application**:
   - In RStudio, open app.R
   - Click the "Run App" button in the top-right corner of the editor
   - Alternatively, run:
     ```R
     shiny::runApp()
     ```

### Method 2: Command Line Installation

1. **Install R**:
   - Download and install R from [CRAN](https://cran.r-project.org/)

2. **Clone or Download the Repository**:
   ```bash
   git clone https://github.com/yourusername/brain-network-analysis.git
   cd brain-network-analysis
   ```

3. **Install Required Packages**:
   ```bash
   Rscript -e "source('setup.R')"
   ```

4. **Run the Application**:
   ```bash
   Rscript -e "shiny::runApp()"
   ```

### Method 3: Docker Installation

1. **Install Docker**:
   - Download and install Docker from [Docker's website](https://www.docker.com/get-started)

2. **Build and Run the Docker Container**:
   ```bash
   # Clone the repository
   git clone https://github.com/yourusername/brain-network-analysis.git
   cd brain-network-analysis
   
   # Build the Docker image
   docker build -t brain-network-analysis .
   
   # Run the container
   docker run -p 3838:3838 brain-network-analysis
   ```

3. **Access the Application**:
   - Open a web browser and navigate to `http://localhost:3838`

## Server Deployment

### Shiny Server (Open Source)

1. **Install R and Shiny Server on Linux**:
   ```bash
   # Install R
   sudo apt-get update
   sudo apt-get install r-base r-base-dev
   
   # Install Shiny and dependencies
   sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""
   
   # Install Shiny Server
   wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.17.973-amd64.deb
   sudo gdebi shiny-server-1.5.17.973-amd64.deb
   ```

2. **Install Required R Packages**:
   ```bash
   sudo su - -c "R -e \"install.packages(c(
     'shinydashboard', 'shinydashboardPlus', 'shinyjs', 'shinyWidgets',
     'DT', 'dplyr', 'ggplot2', 'igraph', 'reshape2', 'tidyr', 'readr', 
     'openxlsx', 'colourpicker', 'RColorBrewer', 'mice', 'naniar', 'ggraph', 
     'scales', 'Cairo', 'viridis', 'plotly', 'htmlwidgets', 'heatmaply',
     'visNetwork', 'markdown', 'knitr', 'rmarkdown', 'zip'
   ), repos='http://cran.rstudio.com/')\""
   ```

3. **Deploy the Application**:
   ```bash
   # Copy application to Shiny Server directory
   sudo cp -R /path/to/brain-network-analysis /srv/shiny-server/
   
   # Set proper permissions
   sudo chown -R shiny:shiny /srv/shiny-server/brain-network-analysis
   ```

4. **Access the Application**:
   - Open a web browser and navigate to `http://your-server-address:3838/brain-network-analysis/`

### RStudio Connect

1. **Install the rsconnect Package**:
   ```R
   install.packages("rsconnect")
   ```

2. **Configure Your Account**:
   ```R
   rsconnect::setAccountInfo(
     name = "your-account-name",
     token = "your-token",
     secret = "your-secret"
   )
   ```

3. **Deploy the Application**:
   ```R
   rsconnect::deployApp(
     appDir = "/path/to/brain-network-analysis",
     appName = "brain-network-analysis",
     appTitle = "Enhanced Brain Network Analysis"
   )
   ```

### shinyapps.io

1. **Install the rsconnect Package**:
   ```R
   install.packages("rsconnect")
   ```

2. **Configure Your Account**:
   - In RStudio: Tools → Global Options → Publishing → Connect
   - Click "Add Account" and follow the instructions
   - Alternatively, use the code method:
     ```R
     rsconnect::setAccountInfo(
       name = "your-account-name",
       token = "your-token",
       secret = "your-secret"
     )
     ```

3. **Deploy the Application**:
   - In RStudio: Click the "Publish" button in the top-right corner of the app.R editor
   - Alternatively, use the code method:
     ```R
     rsconnect::deployApp()
     ```

## Troubleshooting

### Common Installation Issues

1. **Package Installation Failures**:
   - Error: **"Package X is not available for R version Y"**
     - Solution: Ensure you're using a compatible R version
     - Try installing from source: `install.packages("packagename", type = "source")`

2. **Missing System Dependencies**:
   - Linux: Install system dependencies
     ```bash
     sudo apt-get install libssl-dev libcurl4-openssl-dev libxml2-dev
     ```
   - macOS: Install Xcode command line tools
     ```bash
     xcode-select --install
     ```

3. **Memory Limitations**:
   - Error: **"Cannot allocate vector of size X"**
     - Solution: Increase R memory limit
     ```R
     memory.limit(size = 8000)  # Windows only, size in MB
     ```
     - For Linux/macOS, run R with increased memory:
     ```bash
     R --max-mem-size=8000M
     ```

4. **Shiny Server Issues**:
   - Check Shiny Server logs:
     ```bash
     sudo cat /var/log/shiny-server.log
     ```
   - Restart Shiny Server:
     ```bash
     sudo systemctl restart shiny-server
     ```

## Advanced Configuration

### Performance Optimization

1. **Increase R Memory Limit**:
   - Add to your .Rprofile or at the start of app.R:
     ```R
     if (.Platform$OS.type == "windows") {
       memory.limit(size = 8000)  # 8GB, adjust as needed
     }
     ```

2. **Enable Multithreading**:
   - Add to app.R:
     ```R
     options(shiny.usecairo = TRUE)  # For better plotting performance
     options(future.globals.maxSize = 1000 * 1024^2)  # 1GB for future package
     ```

3. **Preload Large Packages**:
   - Create a global.R file with:
     ```R
     library(igraph)
     library(ggplot2)
     # Preload other large packages
     ```

### Custom Configuration

Create a `config.R` file with your specific configuration parameters:

```R
# Example config.R
CONFIG <- list(
  max_upload_size = 100 * 1024^2,  # 100MB
  default_correlation_method = "spearman",
  default_threshold = 0.3,
  cache_dir = "./cache",
  enable_advanced_features = TRUE
)
```

Then source this file in app.R:

```R
source("config.R")
```

## Uninstallation

1. **Remove the Application Directory**:
   ```bash
   rm -rf /path/to/brain-network-analysis
   ```

2. **Optionally Remove Installed Packages**:
   ```R
   remove.packages(c(
     "shiny", "shinydashboard", "shinydashboardPlus", "shinyjs", 
     # ... other packages
   ))
   ```

## Getting Help

If you encounter issues not covered in this guide:

1. Check the [GitHub Issues](https://github.com/yourusername/brain-network-analysis/issues) for similar problems
2. Create a new issue with detailed information about your problem
3. Contact the maintainer at your-email@example.com
