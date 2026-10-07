# Summarize Network by Clusters

Creates a summary network where each cluster becomes a single node. Edge
weights are aggregated from the original network using the specified
method, without normalization.

## Usage

``` r
summarize_network(
  x,
  cluster_list = NULL,
  method = c("sum", "mean", "max", "min", "median", "density", "geomean"),
  directed = TRUE
)

cnet(
  x,
  cluster_list = NULL,
  method = c("sum", "mean", "max", "min", "median", "density", "geomean"),
  directed = TRUE
)
```

## Arguments

- x:

  A weight matrix, tna object, or cograph_network.

- cluster_list:

  Cluster specification:

  - Named list of node vectors (e.g.,
    `list(A = c("n1", "n2"), B = c("n3", "n4"))`)

  - A membership vector or a data frame, as in
    [`csum`](https://sonsoles.me/cograph/reference/csum.md)

  - A single string naming a column of the node table of a
    cograph_network (e.g., "clusters", "groups")

  - NULL (default) to use the first node column named "clusters",
    "cluster", "groups", "group", "community" or "module" of a
    cograph_network, with a message naming the column

- method:

  Aggregation method for edge weights: "sum", "mean", "max", "min",
  "median", "density", "geomean". Default "sum".

- directed:

  Logical. Whether the summary network is directed. Default TRUE.

## Value

A cograph_network object with one node per cluster, labelled by cluster
name. The edge weights are the aggregated between-cluster weights, and
the diagonal holds the aggregated within-cluster weights. The node table
has a `size` column with the number of original nodes in each cluster.

See `summarize_network`.

## See also

[`csum`](https://sonsoles.me/cograph/reference/csum.md),
[`plot_mcml`](https://sonsoles.me/cograph/reference/plot_mcml.md)

## Examples

``` r
clusters <- list(C1 = c("Explore", "Reflect", "Discuss"),
                 C2 = c("Plan", "Create", "Share"),
                 C3 = c("Monitor", "Adapt", "Synthesize", "Evaluate"))
summarize_network(regulation_net, cluster_list = clusters)
#> Cograph network: 3 nodes, 9 edges ( directed )
#> Source: matrix 
#>   Nodes (3): C1, C2, C3
#>   Edges: 6 / 6 (density: 100.0%)
#>   Weights: [0.150, 2.060]  |  mean: 0.792
#>   Strongest edges:
#>     C2 -> C3  2.060
#>     C3 -> C1  1.110
#>     C2 -> C1  0.540
#>     C3 -> C2  0.480
#>     C1 -> C2  0.410
#>   Self-loops: 3  |  range: [1.000, 1.160]
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
