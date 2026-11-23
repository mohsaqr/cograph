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
to a maximum of 1, the divisor is the largest possible sum, \\n - 2\\ on
an undirected network and \\n - 1\\ on a directed one, and a star gives
a value below 1. Weighted closeness scales with the inverse of the edge
weights, so its centralization can exceed 1. Nodes whose score is `NA`
or `NaN` are dropped from the sum. The value is 0 when the network has
two or fewer nodes.

## Details

A weighted input carries its weights into betweenness, closeness and
eigenvector centrality; degree centralization ignores them.

The theoretical maxima (Freeman 1979) are those of
`igraph::centr_degree_tmax(loops = FALSE)`,
[`igraph::centr_betw_tmax()`](https://r.igraph.org/reference/centr_betw_tmax.html)
and
[`igraph::centr_clo_tmax()`](https://r.igraph.org/reference/centr_clo_tmax.html).
Degree: \\(n - 1)(n - 2)\\ on an undirected network; on a directed one
\\(n - 1)^2\\ for in- or out-degree and \\2(n - 1)(n - 2)\\ for in + out
degree (`mode = "all"`, reached by a star with reciprocated arcs).
Betweenness: \\(n - 1)^2 (n - 2) / 2\\ undirected and \\(n - 1)^2 (n -
2)\\ directed. Closeness: \\(n - 1)(n - 2) / (2n - 3)\\, and \\(n - 1)^2
/ n\\ for `mode = "in"` or `"out"` on a directed network. Eigenvector:
\\n - 2\\ undirected and \\n - 1\\ directed, as
[`igraph::centr_eigen_tmax()`](https://r.igraph.org/reference/centr_eigen_tmax.html)
(before 2.7.20 cograph divided by \\n - 1\\ on every network).

## References

Freeman, L. C. (1979). Centrality in social networks: Conceptual
clarification. Social Networks, 1(3), 215-239.
[doi:10.1016/0378-8733(78)90021-7](https://doi.org/10.1016/0378-8733%2878%2990021-7)
.

## Examples

``` r
cograph::centralization(regulation_net, measure = "degree")
#> [1] 0.1388889
```
