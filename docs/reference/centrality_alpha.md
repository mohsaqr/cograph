# Alpha (Katz) Centrality

Influence via all paths penalized by distance. Similar to eigenvector
centrality but includes an exogenous contribution, making it
well-defined even for directed acyclic graphs.

## Usage

``` r
centrality_alpha(x, mode = "all", ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"in"`, or `"out"`.

- ...:

  Additional arguments passed to
  [`centrality`](http://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `normalized`, `weighted`, `directed`).

## Value

Named numeric vector of alpha centrality values.

## See also

[`centrality`](http://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_eigenvector`](http://sonsoles.me/cograph/reference/centrality_eigenvector.md)
for a related measure.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_alpha(adj)
#>  A  B  C 
#> -1 -1 -1 
```
