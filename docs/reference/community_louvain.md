# Louvain Community Detection

Multi-level modularity optimization using the Louvain algorithm. Fast
and widely used for large networks.

## Usage

``` r
community_louvain(x, weights = NULL, resolution = 1, seed = NULL, ...)

com_lv(x, weights = NULL, resolution = 1, seed = NULL, ...)
```

## Arguments

- x:

  Network input

- weights:

  Edge weights. NULL uses network weights, NA for unweighted.

- resolution:

  Resolution parameter. Higher values = more communities. Default 1
  (standard modularity).

- seed:

  Random seed for reproducibility. Default NULL.

- ...:

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md)

## Value

A `cograph_communities` object

A `cograph_communities` object. See
[`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md).

## References

Blondel, V.D., Guillaume, J.L., Lambiotte, R., & Lefebvre, E. (2008).
Fast unfolding of communities in large networks. *Journal of Statistical
Mechanics*, P10008.

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::make_graph("Zachary")
  comm <- community_louvain(g)
  membership(comm)

  # Reproducible result with seed
  comm1 <- community_louvain(g, seed = 42)
  comm2 <- community_louvain(g, seed = 42)
  identical(membership(comm1), membership(comm2))
}
#> [1] TRUE
```
