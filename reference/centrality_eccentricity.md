# Eccentricity

Maximum shortest path distance from a node to any other node. For
directed networks, `centrality_ineccentricity` and
`centrality_outeccentricity` use incoming and outgoing paths.

## Usage

``` r
centrality_eccentricity(x, mode = "all", ...)

centrality_ineccentricity(x, ...)

centrality_outeccentricity(x, ...)
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

Named numeric vector of eccentricity values.

## See also

[`centrality`](http://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_eccentricity(adj)
#> A B C 
#> 2 1 2 
```
