# LeaderRank Centrality

PageRank variant with a ground node connected to all nodes. Requires a
directed graph.

## Usage

``` r
centrality_leaderrank(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).
  Must be directed.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of LeaderRank values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md)
for standard PageRank.

## Examples

``` r
if (FALSE) { # \dontrun{
adj <- matrix(c(0, 1, 0, 0, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_leaderrank(adj)
} # }
```
