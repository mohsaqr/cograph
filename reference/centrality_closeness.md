# Closeness Centrality

Inverse of the average shortest path distance from a node to all others.
For directed networks, `centrality_incloseness` and
`centrality_outcloseness` measure incoming and outgoing closeness.

## Usage

``` r
centrality_closeness(x, mode = "all", ...)

centrality_incloseness(x, ...)

centrality_outcloseness(x, ...)
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

Named numeric vector of closeness values.

## See also

[`centrality`](http://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_harmonic`](http://sonsoles.me/cograph/reference/centrality_harmonic.md)
for a variant that handles disconnected graphs.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_closeness(adj)
#>   A   B   C 
#> 0.5 0.5 0.5 
```
