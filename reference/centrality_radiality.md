# Radiality Centrality

Centrality based on sum of (diameter + 1 - distance) normalized by n-1.
Nodes closer to others (on average) have higher radiality.

## Usage

``` r
centrality_radiality(x, mode = "all", ...)
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

Named numeric vector of radiality values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md)
for a related measure.

## Examples

``` r
adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_radiality(adj)
#>   A   B   C 
#> 3.0 3.5 3.0 
```
