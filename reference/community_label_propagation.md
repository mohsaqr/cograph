# Label Propagation Community Detection

Fast semi-synchronous label propagation algorithm. Each node adopts the
most frequent label among its neighbors.

## Usage

``` r
community_label_propagation(
  x,
  weights = NULL,
  mode = c("out", "in", "all"),
  initial = NULL,
  fixed = NULL,
  seed = NULL,
  ...
)

com_lp(
  x,
  weights = NULL,
  mode = c("out", "in", "all"),
  initial = NULL,
  fixed = NULL,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  Network input

- weights:

  Edge weights. NULL uses network weights, NA for unweighted.

- mode:

  For directed graphs: "out" (default), "in", or "all".

- initial:

  Initial labels (integer vector or NULL for unique labels).

- fixed:

  Logical vector indicating which labels are fixed.

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

Raghavan, U.N., Albert, R., & Kumara, S. (2007). Near linear time
algorithm to detect community structures in large-scale networks.
*Physical Review E*, 76, 036106.

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::make_graph("Zachary")

  # Basic label propagation
  comm <- community_label_propagation(g)

  # With some nodes fixed to specific communities
  initial <- rep(NA, igraph::vcount(g))
  initial[1] <- 1  # Node 1 in community 1
  initial[34] <- 2 # Node 34 in community 2
  fixed <- !is.na(initial)
  initial[is.na(initial)] <- seq_len(sum(is.na(initial)))
  comm2 <- community_label_propagation(g, initial = initial, fixed = fixed)
}
net <- as_cograph(matrix(runif(25), 5, 5))
com_lp(net)
#> Community structure (label_propagation)
#>   Nodes: 5  | Communities: 1  | Modularity: 0 
#>   Sizes: 5 
#> 
#>  node community
#>     1         1
#>     2         1
#>     3         1
#>     4         1
#>     5         1
```
