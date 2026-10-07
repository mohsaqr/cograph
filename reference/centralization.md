# Centralization index

Computes Freeman's centralization for degree, betweenness, closeness, or
eigenvector centrality.

## Usage

``` r
centralization(
  x,
  measure = c("degree", "betweenness", "closeness", "eigenvector"),
  directed = NULL,
  mode = "all",
  ...
)
```

## Arguments

- x:

  Network input (matrix, edge-list data frame, igraph, network,
  cograph_network, tna object).

- measure:

  One of `"degree"` (default), `"betweenness"`, `"closeness"` or
  `"eigenvector"`.

- directed:

  Logical or `NULL`. `NULL` (default) auto-detects from matrix symmetry;
  `TRUE`/`FALSE` forces it.

- mode:

  For directed networks: `"all"` (default), `"in"` or `"out"`. Used by
  `"degree"` and `"closeness"` only.

- ...:

  Ignored; accepted for call compatibility with the other centrality
  verbs.

## Value

A single number. It is the summed gap between the most central node and
every other node, divided by a theoretical maximum. For degree,
betweenness and closeness the maximum is the value of an unweighted
star, so unweighted input gives 0 for a perfectly even network and 1 for
an undirected star. For eigenvector centrality, whose scores are scaled
to a maximum of 1, the divisor is \\n - 1\\, and a star gives a value
below 1. Weighted closeness scales with the inverse of the edge weights,
so its centralization can exceed 1. Nodes whose score is `NA` or `NaN`
are dropped from the sum. The value is 0 when the network has two or
fewer nodes.

## Details

A weighted input carries its weights into betweenness, closeness and
eigenvector centrality; degree centralization ignores them.

## Examples

``` r
cograph::centralization(regulation_net, measure = "degree")
#> [1] 0.2469136
```
