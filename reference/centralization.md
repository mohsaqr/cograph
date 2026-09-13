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

A single number: the summed gap between the most central node and every
other node, divided by the theoretical maximum for the measure, so 0
marks a perfectly even network and 1 a perfect star. Nodes whose score
is `NA` or `NaN` are dropped from the sum. Returns 0 when the network
has two or fewer nodes.

## Details

A weighted input carries its weights into betweenness, closeness and
eigenvector centrality; degree centralization ignores them.

## Examples

``` r
star <- matrix(0, 5, 5)
star[1, 2:5] <- 1; star[2:5, 1] <- 1
cograph::centralization(star, "degree")
#> [1] 1
```
