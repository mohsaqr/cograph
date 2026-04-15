# Lin Centrality

Reachable nodes squared divided by sum of distances. Well-defined for
disconnected graphs.

## Usage

``` r
centrality_lin(x, mode = "all", ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"in"`, or `"out"`.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `normalized`, `weighted`, `directed`).

## Value

Named numeric vector of Lin centrality values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md)
for a related measure.

## Examples

``` r
adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_lin(adj)
#>        A        B        C 
#> 1.333333 2.000000 1.333333 
```
