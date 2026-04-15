# Harmonic Centrality

Sum of inverse shortest path distances to all other nodes. Unlike
closeness, harmonic centrality handles disconnected graphs naturally
(unreachable nodes contribute 0 instead of making the measure
undefined).

## Usage

``` r
centrality_harmonic(x, mode = "all", ...)

centrality_inharmonic(x, ...)

centrality_outharmonic(x, ...)
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

Named numeric vector of harmonic centrality values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md)
for the traditional variant.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_harmonic(adj)
#> A B C 
#> 2 2 2 
```
