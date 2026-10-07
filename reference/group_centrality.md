# Group Centrality (Everett-Borgatti 1999)

Computes the centrality of a set of nodes \\C \subseteq V\\. Distances
are unweighted hop counts in the direction of the edges. The measures
are defined as follows.

## Usage

``` r
group_centrality(
  x,
  nodes,
  measure = c("betweenness", "closeness", "degree"),
  mode = c("all", "out", "in"),
  normalized = TRUE
)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- nodes:

  Integer vector of node indices (1-based) or character vector of node
  names identifying the group \\C\\.

- measure:

  One of `"betweenness"` (default), `"closeness"`, or `"degree"`.

- mode:

  For directed graphs with `measure = "degree"`: `"all"` (both
  directions, default), `"out"` (outgoing), or `"in"` (incoming).
  Ignored for undirected graphs and other measures.

- normalized:

  Logical, for `"betweenness"` only. If `TRUE` (default), divide by
  \\(\|V\| - \|C\|)(\|V\| - \|C\| - 1)\\.

## Value

Numeric scalar: the group centrality of the set `nodes`. Unknown node
names and out-of-range indices raise an error.

## Details

- betweenness:

  \\GBC(C) = \sum\_{s,t \in V \setminus C, s \ne t} \sigma(s, t \mid C)
  / \sigma(s, t)\\, where \\\sigma(s, t)\\ is the number of shortest
  \\s\\-\\t\\ paths and \\\sigma(s, t \mid C)\\ is the number of those
  paths passing through at least one node in \\C\\. Normalized by \\1 /
  ((\|V\| - \|C\|)(\|V\| - \|C\| - 1))\\.

- closeness:

  \\GCC(C) = (\|V\| - \|C\|) / \sum\_{v \in V \setminus C} d(v, C)\\,
  where \\d(v, C) = \min\_{c \in C} d(v, c)\\ is the shortest distance
  from \\v\\ to any group member. Unreachable nodes contribute 0 to the
  denominator sum. For directed graphs, \\d(v, c)\\ follows the edges
  from \\v\\ to \\c\\.

- degree:

  \\GDC(C) = \|N(C) \setminus C\| / (\|V\| - \|C\|)\\, the fraction of
  non-group nodes adjacent to at least one group member. For directed
  graphs, `mode` selects the neighborhood.

## Group betweenness

Group betweenness is computed directly from the Everett and Borgatti
definition, counting the shortest paths that pass through at least one
node in \\C\\. On some graphs the result differs from
`networkx.group_betweenness_centrality`, which uses the iterative
algorithm of Puzis, Elovici and Dolev.

## References

Everett, M. G., & Borgatti, S. P. (1999). The centrality of groups and
classes. *Journal of Mathematical Sociology*, 23(3), 181-201.

Puzis, R., Elovici, Y., & Dolev, S. (2007). Fast algorithm for
successive computation of group betweenness centrality. *Physical Review
E*, 76, 056709.
[doi:10.1103/PhysRevE.76.056709](https://doi.org/10.1103/PhysRevE.76.056709)
.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
per-node measures.

## Examples

``` r
group_centrality(regulation_net, nodes = c("Plan", "Monitor"), measure = "betweenness")
#> [1] 0.3428571
```
