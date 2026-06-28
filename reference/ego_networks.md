# Ego-Network Metrics

Extracts the ego network of each requested node (the node, its
neighbours up to a given order, and the ties among them) and reports a
tidy table of personal-network metrics: size, internal tie counts and
densities, and Burt's structural-hole measures. One row per ego.

## Usage

``` r
ego_networks(
  x,
  nodes = NULL,
  order = 1,
  mode = c("all", "out", "in"),
  directed = NULL,
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object.

- nodes:

  Character vector of node names or integer vector of node indices
  selecting which egos to report. NULL (default) uses every node.

- order:

  Integer neighbourhood order defining the ego network. 1 (default) is
  the standard ego network (ego + direct neighbours). Burt's
  `effective_size` and `constraint` are only defined for `order = 1` and
  are returned as `NA` otherwise.

- mode:

  For directed networks, which ties define the neighbourhood: `"all"`
  (default), `"out"`, or `"in"`.

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.

- ...:

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md).

## Value

A tidy data.frame of class `"cograph_ego_networks"` with one row per ego
and columns:

- node:

  Ego node name.

- size:

  Number of alters (ego-network size, excluding ego).

- ego_ties:

  Number of edges in the ego network (ego + alters).

- ego_density:

  Edge density of the ego network including ego.

- alter_ties:

  Number of edges among the alters only (excluding ego).

- alter_density:

  Edge density among the alters. Low values indicate many structural
  holes / brokerage opportunities.

- effective_size:

  Burt's effective size of the ego network (`order = 1` only).

- constraint:

  Burt's constraint (`order = 1` only).

## Details

`effective_size` and `constraint` are computed on the full network
(Burt's measures are defined directly from each node's order-1 ego
network), reusing the same implementations as
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) so
results match
`centrality(x, measures = c("effective_size", "constraint"))`.

## References

Burt, R.S. (1992). *Structural Holes: The Social Structure of
Competition*. Harvard University Press.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) (for
`effective_size`, `constraint`, `dispersion`),
[`select_neighbors`](https://sonsoles.me/cograph/reference/select_neighbors.md),
[`neighborhood_overlap`](https://sonsoles.me/cograph/reference/neighborhood_overlap.md)

## Examples

``` r
adj <- matrix(c(
  0, 1, 1, 0, 0,
  1, 0, 1, 0, 0,
  1, 1, 0, 1, 1,
  0, 0, 1, 0, 1,
  0, 0, 1, 1, 0
), 5, 5, byrow = TRUE)
rownames(adj) <- colnames(adj) <- LETTERS[1:5]
cograph::ego_networks(adj)
#> Ego Networks (order = 1, mode = all)
#> ================================================== 
#>  node size ego_ties ego_density alter_ties alter_density effective_size
#>     A    2        3         1.0          1     1.0000000              1
#>     B    2        3         1.0          1     1.0000000              1
#>     C    4        6         0.6          2     0.3333333              3
#>     D    2        3         1.0          1     1.0000000              1
#>     E    2        3         1.0          1     1.0000000              1
#>  constraint
#>    0.953125
#>    0.953125
#>    0.562500
#>    0.953125
#>    0.953125
```
