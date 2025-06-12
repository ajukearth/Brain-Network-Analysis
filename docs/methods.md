# Methods Reference

This document provides detailed information about the mathematical and computational methods used in the Enhanced Brain Network Analysis application.

## Table of Contents

1. [Correlation Methods](#correlation-methods)
2. [Network Construction](#network-construction)
3. [Network Metrics](#network-metrics)
4. [Community Detection](#community-detection)
5. [Multi-Method Consensus](#multi-method-consensus)
6. [Statistical Approaches](#statistical-approaches)
7. [References](#references)

## Correlation Methods

### Pearson Correlation

Pearson correlation measures the linear relationship between two variables.

**Formula**:
$r_{xy} = \frac{\sum_{i=1}^{n} (x_i - \bar{x})(y_i - \bar{y})}{\sqrt{\sum_{i=1}^{n} (x_i - \bar{x})^2 \sum_{i=1}^{n} (y_i - \bar{y})^2}}$

Where:
- $x_i$ and $y_i$ are individual observations
- $\bar{x}$ and $\bar{y}$ are the means of the variables
- $n$ is the number of observations

**Characteristics**:
- Assumes normally distributed data
- Sensitive to outliers
- Detects linear relationships only
- Values range from -1 (perfect negative correlation) to 1 (perfect positive correlation)

### Spearman Correlation

Spearman's rank correlation assesses monotonic relationships between variables.

**Formula**:
$\rho = 1 - \frac{6 \sum d_i^2}{n(n^2 - 1)}$

Where:
- $d_i$ is the difference between ranks of corresponding variables
- $n$ is the number of observations

**Characteristics**:
- Non-parametric (does not assume normal distribution)
- Less sensitive to outliers than Pearson
- Detects monotonic relationships (not just linear)
- Values range from -1 to 1

### Kendall's Tau

Kendall's tau measures ordinal association between variables.

**Formula**:
$\tau = \frac{C - D}{\binom{n}{2}}$

Where:
- $C$ is the number of concordant pairs
- $D$ is the number of discordant pairs
- $\binom{n}{2}$ is the total number of possible pairs

**Characteristics**:
- Non-parametric
- More robust to outliers than Spearman
- Better for small sample sizes
- Values range from -1 to 1

### Partial Correlation

Partial correlation measures the relationship between two variables while controlling for the effects of one or more other variables.

**Formula**:
$r_{xy.z} = \frac{r_{xy} - r_{xz}r_{yz}}{\sqrt{(1 - r_{xz}^2)(1 - r_{yz}^2)}}$

Where:
- $r_{xy}$ is the correlation between variables $x$ and $y$
- $r_{xz}$ is the correlation between variables $x$ and $z$
- $r_{yz}$ is the correlation between variables $y$ and $z$

**Characteristics**:
- Removes indirect influences
- Helps identify direct relationships
- Can significantly change network structure compared to standard correlation

## Network Construction

### Correlation Matrix

The correlation matrix forms the basis of network construction:

1. Calculate pairwise correlations between all brain regions
2. Arrange correlations in a symmetric matrix where:
   - Rows and columns represent brain regions
   - Each cell contains the correlation coefficient between two regions

### Thresholding

Several thresholding approaches are implemented:

#### 1. Absolute Threshold

Retain only connections with correlation magnitude above a user-defined threshold:

$A_{ij} = \begin{cases} 
1 & \text{if } |r_{ij}| \geq \tau \\
0 & \text{otherwise}
\end{cases}$

Where:
- $A_{ij}$ is the adjacency matrix element
- $r_{ij}$ is the correlation coefficient
- $\tau$ is the threshold value

#### 2. Proportional Threshold

Retain a fixed proportion of the strongest connections:

1. Sort all correlation values by magnitude
2. Keep the top $p\%$ of connections
3. Set all other connections to zero

#### 3. Statistical Threshold

Retain only statistically significant correlations:

1. Calculate p-values for each correlation
2. Apply multiple comparison correction (e.g., FDR, Bonferroni)
3. Retain connections with corrected p-values below significance level

### Weighted vs. Binary Networks

The application supports both weighted and binary network representations:

**Binary Networks**:
- Connections are either present (1) or absent (0)
- Simplifies network structure and some calculations
- Loses information about connection strength

**Weighted Networks**:
- Connections retain their correlation values
- Preserves information about connection strength
- More computationally intensive for some metrics

## Network Metrics

### Global Metrics

#### Network Density

The proportion of actual connections relative to all possible connections:

$D = \frac{2E}{N(N-1)}$

Where:
- $E$ is the number of edges
- $N$ is the number of nodes

#### Global Clustering Coefficient

Measures the tendency of nodes to cluster together:

$C = \frac{1}{N} \sum_{i=1}^{N} C_i$

Where:
- $C_i$ is the local clustering coefficient of node $i$
- $N$ is the number of nodes

#### Characteristic Path Length

The average shortest path length between all pairs of nodes:

$L = \frac{1}{N(N-1)} \sum_{i \neq j} d_{ij}$

Where:
- $d_{ij}$ is the shortest path length between nodes $i$ and $j$
- $N$ is the number of nodes

#### Small-World Index

Quantifies the "small-world-ness" of a network:

$\sigma = \frac{C/C_{rand}}{L/L_{rand}}$

Where:
- $C$ is the clustering coefficient of the network
- $C_{rand}$ is the clustering coefficient of a random network
- $L$ is the characteristic path length of the network
- $L_{rand}$ is the characteristic path length of a random network

#### Modularity

Measures the strength of division of a network into modules:

$Q = \frac{1}{2m} \sum_{ij} \left[ A_{ij} - \frac{k_i k_j}{2m} \right] \delta(c_i, c_j)$

Where:
- $A_{ij}$ is the adjacency matrix element
- $k_i$ and $k_j$ are the degrees of nodes $i$ and $j$
- $m$ is the total number of edges
- $\delta(c_i, c_j)$ is 1 if nodes $i$ and $j$ are in the same community, 0 otherwise

### Node-Level Metrics

#### Degree Centrality

The number of connections a node has:

$k_i = \sum_{j=1}^{N} A_{ij}$

Where:
- $A_{ij}$ is the adjacency matrix element
- $N$ is the number of nodes

For weighted networks, this becomes strength:

$s_i = \sum_{j=1}^{N} W_{ij}$

Where:
- $W_{ij}$ is the weight of the connection between nodes $i$ and $j$

#### Betweenness Centrality

Measures how often a node lies on the shortest path between other nodes:

$b_i = \sum_{s \neq i \neq t} \frac{\sigma_{st}(i)}{\sigma_{st}}$

Where:
- $\sigma_{st}$ is the total number of shortest paths from node $s$ to node $t$
- $\sigma_{st}(i)$ is the number of those paths that pass through node $i$

#### Closeness Centrality

Measures how close a node is to all other nodes in the network:

$c_i = \frac{N-1}{\sum_{j=1}^{N} d_{ij}}$

Where:
- $d_{ij}$ is the shortest path length between nodes $i$ and $j$
- $N$ is the number of nodes

#### Eigenvector Centrality

Measures the influence of a node based on the centrality of its neighbors:

$x_i = \frac{1}{\lambda} \sum_{j=1}^{N} A_{ij} x_j$

Where:
- $A_{ij}$ is the adjacency matrix element
- $\lambda$ is the largest eigenvalue of the adjacency matrix
- $x_j$ is the eigenvector centrality of node $j$

#### Participation Coefficient

Measures how well a node's connections are distributed across different modules:

$P_i = 1 - \sum_{s=1}^{N_M} \left( \frac{k_{is}}{k_i} \right)^2$

Where:
- $k_{is}$ is the number of links from node $i$ to module $s$
- $k_i$ is the total degree of node $i$
- $N_M$ is the number of modules

#### Within-Module Z-Score

Measures how well-connected a node is to other nodes in its module:

$Z_i = \frac{k_i(s_i) - \bar{k}(s_i)}{\sigma_{k(s_i)}}$

Where:
- $k_i(s_i)$ is the number of links from node $i$ to other nodes in its module $s_i$
- $\bar{k}(s_i)$ is the average of $k_j(s_i)$ over all nodes $j$ in module $s_i$
- $\sigma_{k(s_i)}$ is the standard deviation of $k_j(s_i)$ in module $s_i$

## Community Detection

### Louvain Algorithm

A hierarchical algorithm that optimizes modularity:

1. Start with each node in its own community
2. For each node, calculate the modularity gain of moving it to a neighbor's community
3. Place the node in the community that maximizes modularity gain
4. Repeat until no further improvement is possible
5. Create a new network where nodes are the communities found
6. Repeat steps 1-5 until no further improvement is possible

### Walktrap Algorithm

Uses random walks to identify communities:

1. Perform random walks of a fixed length (typically 3-5 steps)
2. Calculate distance between nodes based on the probability of reaching one node from another
3. Use hierarchical clustering to group nodes with similar distances
4. Cut the dendrogram at the level that maximizes modularity

### Infomap Algorithm

Uses information theory to detect communities:

1. Model information flow as random walks on the network
2. Encode the random walks using a two-level description:
   - First level: modules (communities)
   - Second level: nodes within modules
3. Find the partition that minimizes the description length

### Consensus Clustering

Combines multiple community detection runs:

1. Run community detection multiple times (with different parameters or algorithms)
2. Create a consensus matrix where each element represents the proportion of runs in which two nodes were placed in the same community
3. Apply a threshold to the consensus matrix
4. Run community detection on the thresholded consensus matrix to obtain final communities

## Multi-Method Consensus

### Method Integration

Several approaches for combining results from different correlation methods:

#### 1. Rank Aggregation

1. For each correlation method, rank all possible connections by strength
2. Compute the average rank for each connection across methods
3. Select connections with the highest average ranks

#### 2. Weighted Average

1. Normalize correlation matrices from different methods
2. Compute a weighted average of the normalized matrices
3. Apply thresholding to the resulting consensus matrix

#### 3. Minimum Spanning Tree Union

1. Extract the minimum spanning tree (MST) from each correlation matrix
2. Take the union of all MSTs as a backbone network
3. Add additional edges based on consensus strength

#### 4. Consensus Threshold

1. Apply a threshold to each correlation matrix individually
2. Count how many methods retain each connection
3. Keep connections that are present in at least $k$ methods

## Statistical Approaches

### Multiple Comparison Correction

#### Bonferroni Correction

Controls family-wise error rate by adjusting the significance threshold:

$\alpha_{adj} = \frac{\alpha}{m}$

Where:
- $\alpha$ is the original significance level
- $m$ is the number of comparisons
- $\alpha_{adj}$ is the adjusted significance level

#### False Discovery Rate (FDR)

Controls the expected proportion of false positives:

1. Sort p-values in ascending order: $p_{(1)} \leq p_{(2)} \leq ... \leq p_{(m)}$
2. Find the largest $k$ such that $p_{(k)} \leq \frac{k}{m} \alpha$
3. Reject all hypotheses with p-values $\leq p_{(k)}$

### Group Comparison Methods

#### Network-Based Statistic (NBS)

Tests for differences in network connections between groups:

1. Perform a statistical test (e.g., t-test) for each connection
2. Apply a primary threshold to identify suprathreshold connections
3. Identify connected components in the suprathreshold network
4. Calculate the size of each component
5. Use permutation testing to determine significance

#### Permutation Testing

Non-parametric approach for significance testing:

1. Calculate the test statistic for the original data
2. Randomly permute group labels many times (e.g., 1000)
3. Calculate the test statistic for each permutation
4. Compute the p-value as the proportion of permutations with test statistics as extreme as or more extreme than the original

## References

1. Rubinov, M., & Sporns, O. (2010). Complex network measures of brain connectivity: uses and interpretations. Neuroimage, 52(3), 1059-1069.

2. Zalesky, A., Fornito, A., & Bullmore, E. T. (2010). Network-based statistic: identifying differences in brain networks. Neuroimage, 53(4), 1197-1207.

3. Bullmore, E., & Sporns, O. (2009). Complex brain networks: graph theoretical analysis of structural and functional systems. Nature reviews neuroscience, 10(3), 186-198.

4. Newman, M. E. (2006). Modularity and community structure in networks. Proceedings of the national academy of sciences, 103(23), 8577-8582.

5. Lancichinetti, A., & Fortunato, S. (2012). Consensus clustering in complex networks. Scientific reports, 2(1), 1-7.

6. Fornito, A., Zalesky, A., & Breakspear, M. (2013). Graph analysis of the human connectome: promise, progress, and pitfalls. Neuroimage, 80, 426-444.

7. Zalesky, A., Fornito, A., Cocchi, L., Gollo, L. L., & Breakspear, M. (2014). Time-resolved resting-state brain networks. Proceedings of the National Academy of Sciences, 111(28), 10341-10346.

8. Blondel, V. D., Guillaume, J. L., Lambiotte, R., & Lefebvre, E. (2008). Fast unfolding of communities in large networks. Journal of statistical mechanics: theory and experiment, 2008(10), P10008.

9. Smith, S. M., Miller, K. L., Salimi-Khorshidi, G., Webster, M., Beckmann, C. F., Nichols, T. E., ... & Woolrich, M. W. (2011). Network modelling methods for FMRI. Neuroimage, 54(2), 875-891.

10. van den Heuvel, M. P., & Sporns, O. (2013). Network hubs in the human brain. Trends in cognitive sciences, 17(12), 683-696.
