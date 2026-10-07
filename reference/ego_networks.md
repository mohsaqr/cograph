# Ego-Network Metrics

Extracts the ego network of each requested node and computes a table of
personal-network metrics with one row per ego. An ego network consists
of the node, its neighbors up to a given order and the ties among them.
The metrics are the network size, tie counts and densities, and Burt's
structural-hole measures.

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
  selecting which egos to report. NULL (default) uses every node. An
  unknown name or an index out of range raises an error.

- order:

  Integer neighborhood order defining the ego network. 1 (default) is
  the standard ego network (ego and direct neighbors). Burt's
  `effective_size` and `constraint` are only defined for `order = 1` and
  are returned as `NA` otherwise.

- mode:

  For directed networks, which ties define the neighborhood: `"all"`
  (default), `"out"`, or `"in"`. The Burt measures do not depend on
  `mode`.

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  which takes no further arguments, so any argument supplied here raises
  an error.

## Value

A data frame of class `"cograph_ego_networks"` with one row per ego and
columns:

- node:

  Ego node name.

- size:

  Number of alters (ego-network size, excluding ego).

- ego_ties:

  Number of edges in the ego network (ego and alters).

- ego_density:

  Edge density of the ego network including ego. `NA` for an ego without
  alters.

- alter_ties:

  Number of edges among the alters only (excluding ego).

- alter_density:

  Edge density among the alters. `NA` for an ego with fewer than two
  alters. Low values indicate many structural holes and brokerage
  opportunities.

- effective_size:

  Burt's effective size of the ego network (`order = 1` only).

- constraint:

  Burt's constraint (`order = 1` only).

The arguments `order` and `mode` and the directedness of the network are
stored as attributes. Printing the result shows `order` and `mode` above
the table.

## Details

Self-loops are dropped before ties are counted. In a directed network
the tie counts are counts of directed edges, and the densities divide by
\\m(m-1)\\ for \\m\\ members.

`effective_size` and `constraint` are computed on the full network from
all ties of each node, with the same implementations as
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md), so
the values equal
`centrality(x, measures = c("effective_size", "constraint"))`. Effective
size uses the unweighted ties. Constraint uses the edge weights.

## References

Burt, R.S. (1992). *Structural Holes: The Social Structure of
Competition*. Harvard University Press.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) (for
`effective_size` and `constraint`),
[`dispersion`](https://sonsoles.me/cograph/reference/dispersion.md),
[`select_neighbors`](https://sonsoles.me/cograph/reference/select_neighbors.md),
[`neighborhood_overlap`](https://sonsoles.me/cograph/reference/neighborhood_overlap.md)

## Examples

``` r
cograph::ego_networks(regulation_net, nodes = "Plan")
#> Ego Networks (order = 1, mode = all)
#> ================================================== 
#>  node size ego_ties ego_density alter_ties alter_density effective_size
#>  Plan    6       15   0.3571429          8     0.2666667       4.714286
#>  constraint
#>   0.3609471
```
