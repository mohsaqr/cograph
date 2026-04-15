# Bottleneck Centrality

Number of shortest path trees where the node appears in more than n/4
paths.

## Usage

``` r
centrality_bottleneck(x, mode = "all", ...)
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

Named integer vector of bottleneck centrality values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
centrality_bottleneck(adj)
#> A B C D 
#> 0 3 3 0 
```
