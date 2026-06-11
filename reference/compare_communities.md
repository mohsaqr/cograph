# Compare Community Structures

Compares two community structures using various similarity measures.

## Usage

``` r
compare_communities(
  comm1,
  comm2,
  method = c("vi", "nmi", "split.join", "rand", "adjusted.rand")
)
```

## Arguments

- comm1:

  First community structure (communities object or membership vector)

- comm2:

  Second community structure (communities object or membership vector)

- method:

  Comparison method: "vi" (variation of information), "nmi" (normalized
  mutual information), "split.join", "rand" (Rand index),
  "adjusted.rand"

## Value

Numeric similarity/distance value

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::make_graph("Zachary")
  c1 <- community_louvain(g)
  c2 <- community_leiden(g)
  compare_communities(c1, c2, "nmi")
}
#> [1] 0.5449399
```
