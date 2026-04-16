# Local Bridging Centrality

(1/degree) times bridging coefficient. Local measure of inter-community
connectivity.

## Usage

``` r
centrality_local_bridging(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of local bridging values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_bridging`](https://sonsoles.me/cograph/reference/centrality_bridging.md)
for the betweenness-weighted variant.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_local_bridging(adj)
#>    A    B    C 
#> 0.25 0.25 0.25 
```
