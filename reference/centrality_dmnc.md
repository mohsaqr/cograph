# Density of Maximum Neighborhood Component (DMNC)

Edges divided by nodes raised to `dmnc_epsilon`, both taken from the
largest connected component of the subgraph induced on a node's
neighbours (the focal node excluded).

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

## Divergence from centiserve

[`centiserve::dmnc()`](https://rdrr.io/pkg/centiserve/man/dmnc.html)
returns different values, and not only because of its different
`epsilon` default. Its edge count is taken with
`induced.subgraph(graph, which(c$membership %in% ...))`, where the
membership vector indexes the neighbourhood subgraph but is used to
subset the original graph. The two index spaces are not the same, so the
edges counted are those of an unrelated vertex set. On the Zachary
karate club the two disagree on 14 of 34 nodes at a matched epsilon, and
reproducing that indexing exactly reproduces centiserve's output.
cograph counts the edges of the component it actually found.

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
