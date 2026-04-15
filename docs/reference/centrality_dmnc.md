# Density of Maximum Neighborhood Component (DMNC)

Edge count divided by max component size^1.5 in the neighborhood
subgraph.

## Usage

``` r
centrality_dmnc(x, mode = "all", dmnc_epsilon = 1.7, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"in"`, or `"out"`.

- dmnc_epsilon:

  Numeric. Epsilon exponent for DMNC. Default 1.7 as recommended by Lin
  et al. (2008). centiserve uses 1.67 (four-community assumption). Must
  be between 1 and 2.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `normalized`, `weighted`, `directed`).

## Value

Named numeric vector of DMNC values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_mnc`](https://sonsoles.me/cograph/reference/centrality_mnc.md)
for the size-only variant.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_dmnc(adj)
#>         A         B         C 
#> 0.3077861 0.3077861 0.3077861 
```
