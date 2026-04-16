# Subgraph Centrality

Participation in closed loops (walks), weighting shorter loops more
heavily. Based on the diagonal of the matrix exponential of the
adjacency matrix.

## Usage

``` r
centrality_subgraph(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `weighted`, `directed`).

## Value

Named numeric vector of subgraph centrality values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_subgraph(adj)
#>        A        B        C 
#> 2.708272 2.708272 2.708272 
```
