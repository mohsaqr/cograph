# Bonacich Power Centrality

Measures influence based on connections to other influential nodes. The
power parameter controls whether connections to well-connected nodes
increase or decrease centrality.

## Usage

``` r
centrality_power(x, mode = "all", ...)
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

Named numeric vector of power centrality values.

## See also

[`centrality`](http://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_eigenvector`](http://sonsoles.me/cograph/reference/centrality_eigenvector.md)
for a related measure.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_power(adj)
#>  A  B  C 
#> -1 -1 -1 
```
