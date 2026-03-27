# Walktrap Community Detection

Detects communities via random walks. Nodes within the same community
tend to have short random walk distances.

## Usage

``` r
community_walktrap(
  x,
  weights = NULL,
  steps = 4,
  merges = TRUE,
  modularity = TRUE,
  membership = TRUE,
  ...
)

com_wt(
  x,
  weights = NULL,
  steps = 4,
  merges = TRUE,
  modularity = TRUE,
  membership = TRUE,
  ...
)
```

## Arguments

- x:

  Network input

- weights:

  Edge weights. NULL uses network weights, NA for unweighted.

- steps:

  Number of random walk steps. Default 4.

- merges:

  Logical; return merge matrix? Default TRUE.

- modularity:

  Logical; return modularity scores? Default TRUE.

- membership:

  Logical; return membership vector? Default TRUE.

- ...:

  Additional arguments passed to
  [`to_igraph`](http://sonsoles.me/cograph/reference/to_igraph.md)

## Value

A `cograph_communities` object

A `cograph_communities` object. See
[`detect_communities`](http://sonsoles.me/cograph/reference/detect_communities.md).

## References

Pons, P., & Latapy, M. (2006). Computing communities in large networks
using random walks. *Journal of Graph Algorithms and Applications*,
10(2), 191-218.

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::make_graph("Zachary")

  # Default 4 steps
  comm <- community_walktrap(g)

  # More steps for larger communities
  comm2 <- community_walktrap(g, steps = 8)
}
```
