# Maximum Neighborhood Component (MNC)

Size of the largest connected component in the node's neighborhood
subgraph.

## Usage

``` r
centrality_mnc(x, mode = "all", ...)
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

Named integer vector of MNC values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_dmnc`](https://sonsoles.me/cograph/reference/centrality_dmnc.md)
for the density variant.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_mnc(adj)
#> A B C 
#> 2 2 2 
```
