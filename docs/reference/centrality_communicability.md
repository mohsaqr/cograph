# Communicability Centrality

Total communicability: row sums of the matrix exponential of the
adjacency matrix. Measures a node's ability to broadcast information
through all paths.

## Usage

``` r
centrality_communicability(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of communicability values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_subgraph`](https://sonsoles.me/cograph/reference/centrality_subgraph.md)
for the diagonal-only variant.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_communicability(adj)
#>        A        B        C 
#> 7.389056 7.389056 7.389056 
```
