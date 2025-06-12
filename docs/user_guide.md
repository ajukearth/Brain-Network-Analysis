# Enhanced Brain Network Analysis - User Guide

## Overview

This application provides a comprehensive toolkit for analyzing functional connectivity networks in neuroscience research. It follows a modular workflow that allows for flexibility and depth in your analyses, from data import through advanced network analytics and visualization.

## Workflow Summary

1. **Data Import**: Upload and configure your connectivity data
2. **Preprocessing**: Clean, validate, and prepare your data for analysis
3. **Correlation Methods**: Generate connectivity matrices using various correlation methods
4. **Network Analysis**: Calculate graph theory metrics to characterize networks
5. **Visualization**: Create interactive visualizations of networks and metrics
6. **Advanced Analysis**: Perform specialized analyses like bootstrapping or consensus methods
7. **Results**: Export, save, and report your findings

## Detailed Instructions

### 1. Data Import

The data import module allows you to upload your connectivity data and configure it for analysis:

- **Supported Formats**: CSV (.csv) and Excel (.xlsx, .xls) files
- **Required Structure**:
  - At least one ID column (e.g., "Subject")
  - At least one grouping variable (e.g., "Condition", "Sex", "Treatment")
  - Multiple numeric columns representing brain region measurements
  - Optionally, behavioral measures for additional analyses

**Example data structure**:

| Subject | Group1 | Group2 | Region1 | Region2 | Region3 | Behavior1 |
|---------|--------|--------|---------|---------|---------|-----------|
| 1       | Male   | Control| 3.1     | 2.3     | 3.5     | 25.3      |
| 2       | Male   | Stress | 4.2     | 2.8     | 3.9     | 42.1      |
| 3       | Female | Control| 3.8     | 2.6     | 3.6     | 23.8      |

**Import Steps**:

1. Click the "Browse..." button to select your data file
2. For Excel files, select the appropriate sheet
3. Configure columns:
   - Select the Subject/ID column
   - Select Group column(s)
   - Optionally select Behavioral measure column(s)
4. All remaining numeric columns will be treated as brain region data
5. Review the data preview and quality check report
6. Click "Proceed to Preprocessing" to continue

### 2. Preprocessing

The preprocessing module helps you prepare your data for analysis:

- **Missing Data Handling**:
  - View visualization of missing data patterns
  - Select imputation method (mean, median, KNN, MICE)
  - Choose to exclude regions or subjects with excessive missing data
  
- **Outlier Detection**:
  - View boxplots and distribution of values by region
  - Select outlier detection method (IQR, Z-score, MAD)
  - Choose to winsorize, transform, or remove outliers
  
- **Data Transformation**:
  - Apply log, square root, or other transformations to normalize data
  - Z-score standardization options
  
- **Quality Control Checks**:
  - Summary statistics for each region and group
  - Variance checks to identify low-variance regions
  - Correlation checks to identify highly collinear regions

### 3. Correlation Methods

Configure how connectivity matrices are generated:

- **Method Selection**:
  - Choose correlation method (Pearson, Spearman, Kendall)
  - Partial correlation options
  - Set correlation thresholds for network construction
  
- **Multiple Testing Correction**:
  - FDR, Bonferroni, or other correction methods
  - Significance thresholds
  
- **Advanced Options**:
  - Absolute correlation values option
  - Bootstrap confidence intervals
  - Regularization for sparse networks

### 4. Network Analysis

Analyze the network structure using graph theory metrics:

- **Global Metrics**:
  - Density, clustering coefficient, path length
  - Small-worldness, modularity, efficiency
  
- **Node-level Metrics**:
  - Degree, betweenness, closeness centrality
  - Participation coefficient, within-module z-score
  
- **Edge-level Analysis**:
  - Weight distribution
  - Edge betweenness
  
- **Community Detection**:
  - Multiple algorithms (Louvain, Walktrap, etc.)
  - Resolution parameter settings
  - Consensus community detection

### 5. Visualization

Create informative visualizations of your network data:

- **Network Graphs**:
  - Multiple layout options (force-directed, circular, etc.)
  - Color coding by brain area or metric values
  - Size nodes by centrality measures
  
- **Correlation Heatmaps**:
  - Hierarchical clustering options
  - Color palette customization
  - Annotation by brain area
  
- **Metric Visualizations**:
  - Bar charts and box plots of network metrics by group
  - Scatter plots of relationships between metrics
  - Brain area summaries

### 6. Advanced Analysis

Perform specialized analyses for deeper insights:

- **Bootstrapping & Resampling**:
  - Generate confidence intervals for network metrics
  - Assess stability of network properties
  
- **Multi-method Consensus**:
  - Combine results from multiple correlation methods
  - Identify robust network features across methods
  
- **Structure-Function Coupling**:
  - Compare structural and functional connectivity
  - Analyze structure-function relationships
  
- **Statistical Comparisons**:
  - Group comparisons with appropriate statistical tests
  - Correlation with behavioral measures
  - Multiple comparison correction

### 7. Results

Export and report your findings:

- **Data Export Options**:
  - CSV files of all metrics and results
  - Excel workbooks with multiple sheets
  - Network files in various formats (GraphML, etc.)
  
- **Visualization Export**:
  - High-resolution images (PNG, SVG, PDF)
  - Interactive plots for web sharing
  
- **Report Generation**:
  - Summary reports with key findings
  - Methods documentation for reproducibility
  - Configuration settings export

## Best Practices for Network Analysis

1. **Data Quality First**: Ensure your data is clean and well-prepared before analysis
2. **Multiple Methods**: Use multiple correlation methods to verify findings
3. **Threshold Sensitivity**: Test different correlation thresholds to ensure robustness
4. **Statistical Validation**: Apply appropriate statistical tests for group comparisons
5. **Biological Interpretation**: Connect network findings to biological mechanisms
6. **Reproducibility**: Document all steps and parameters for reproducible research

## Troubleshooting

**Common Issues and Solutions**:

- **File Import Errors**: Ensure your file is properly formatted with headers and without special characters
- **Memory Limitations**: For very large datasets, consider preprocessing to reduce dimensionality
- **Visualization Performance**: Reduce the number of nodes or edges for better performance
- **Statistical Power**: Be cautious with interpretations when sample sizes are small
- **Convergence Issues**: For advanced methods, try different initialization parameters

If you encounter persistent issues, check the FAQ or contact support.

## References and Resources

For more information on network neuroscience methods:

- Rubinov, M., & Sporns, O. (2010). Complex network measures of brain connectivity: uses and interpretations. Neuroimage, 52(3), 1059-1069.
- Bullmore, E., & Sporns, O. (2009). Complex brain networks: graph theoretical analysis of structural and functional systems. Nature Reviews Neuroscience, 10(3), 186-198.
- Bassett, D. S., & Sporns, O. (2017). Network neuroscience. Nature Neuroscience, 20(3), 353-364.
