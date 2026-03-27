# Plot Cluster Significance

Creates a histogram of the null distribution with the observed value
marked.

## Usage

``` r
# S3 method for class 'cograph_cluster_significance'
plot(x, ...)
```

## Arguments

- x:

  A `cograph_cluster_significance` object

- ...:

  Additional arguments passed to `hist`

## Value

Invisibly returns x

## Examples

``` r
# \donttest{
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::make_graph("Zachary")
  comm <- community_louvain(g)
  sig <- cluster_significance(g, comm, n_random = 20, seed = 42)
  plot(sig)
}

# }
```
