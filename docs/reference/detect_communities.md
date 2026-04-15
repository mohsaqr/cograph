# Detect Communities in a Network

Detects communities (clusters) in a network using various community
detection algorithms. Returns a data frame with node-community
assignments.

## Usage

``` r
detect_communities(x, method = "louvain", directed = NULL, weights = TRUE)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object.

- method:

  Community detection algorithm to use. One of:

  - `"louvain"`: Louvain method (default, fast and accurate)

  - `"walktrap"`: Walktrap algorithm based on random walks

  - `"fast_greedy"`: Fast greedy modularity optimization

  - `"label_prop"`: Label propagation algorithm

  - `"infomap"`: Infomap algorithm based on information flow

  - `"leiden"`: Leiden algorithm (improved Louvain)

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected.

- weights:

  Logical. Use edge weights for community detection. Default TRUE.

## Value

A data frame with columns:

- `node`: Node labels/names

- `community`: Integer community membership

## Examples

``` r
# Basic usage
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
detect_communities(adj)
#> Community structure (louvain)
#>   Nodes: 4  | Communities: 2  | Modularity: 0.0355 
#>   Sizes: 2, 2 
#> 
#>  node community
#>     A         1
#>     B         2
#>     C         1
#>     D         2

# Different algorithm
detect_communities(adj, method = "walktrap")
#> Community structure (walktrap)
#>   Nodes: 4  | Communities: 2  | Modularity: 0.0355 
#>   Sizes: 2, 2 
#> 
#>  node community
#>     A         2
#>     B         1
#>     C         2
#>     D         1
```
