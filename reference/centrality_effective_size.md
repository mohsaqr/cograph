# Effective Size (Burt's)

Network effective size: degree minus redundancy. Measures non-redundant
contacts in ego network.

## Usage

``` r
centrality_effective_size(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of effective size values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_constraint`](https://sonsoles.me/cograph/reference/centrality_constraint.md)
for a related structural holes measure.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_effective_size(adj)
#> A B C 
#> 1 1 1 
```
