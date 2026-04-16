# Gil-Schmidt Power Index

Sum of 1/d(v,w) normalized by (n-1). Variant of closeness using harmonic
mean of distances.

## Usage

``` r
centrality_gilschmidt(x, mode = "all", ...)
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

Named numeric vector of Gil-Schmidt power index values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md)
for a related measure.

## Examples

``` r
adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
centrality_gilschmidt(adj)
#>         A         B         C         D 
#> 0.6111111 0.8333333 0.8333333 0.6111111 
```
