# Fast Greedy Community Detection

Hierarchical agglomeration using greedy modularity optimization.
Produces a dendrogram of community merges.

## Usage

``` r
community_fast_greedy(
  x,
  weights = NULL,
  merges = TRUE,
  modularity = TRUE,
  membership = TRUE,
  ...
)

com_fg(
  x,
  weights = NULL,
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

- merges:

  Logical; return merge matrix? Default TRUE.

- modularity:

  Logical; return modularity scores? Default TRUE.

- membership:

  Logical; return membership vector? Default TRUE.

- ...:

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md)

## Value

A `cograph_communities` object with optional dendrogram

A `cograph_communities` object. See
[`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md).

## References

Clauset, A., Newman, M.E.J., & Moore, C. (2004). Finding community
structure in very large networks. *Physical Review E*, 70, 066111.

## Examples

``` r
g <- igraph::make_graph("Zachary")
comm <- community_fast_greedy(g)
membership(comm)
#>  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 
#>  1  3  3  3  1  1  1  3  2  3  1  1  3  3  2  2  1  3  2  1  2  3  2  2  2  2 
#> 27 28 29 30 31 32 33 34 
#>  2  2  2  2  2  2  2  2 
```
