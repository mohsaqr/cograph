# Group Centrality (Everett-Borgatti 1999)

Group centrality measures the importance of a *set* of nodes \\C
\subseteq V\\ rather than a single node. Three variants are supported:

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

  One of `"betweenness"`, `"closeness"`, `"degree"`.

- mode:

  For directed graphs with `measure = "degree"`: `"all"` (both
  directions), `"out"` (outgoing), or `"in"` (incoming). Ignored for
  undirected graphs and other measures.

- normalized:

  Logical, for `"betweenness"` only. If `TRUE` (default), divide by
  \\(\|V\| - \|C\|)(\|V\| - \|C\| - 1)\\.

## Value

A single numeric scalar — the group centrality of the set `nodes`.

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
  denominator sum (matching NetworkX convention). For directed graphs,
  cograph uses \\d(v, c)\\ in the original direction, equivalent to
  NetworkX's "reverse then multi-source".

- degree:

  \\GDC(C) = \|N(C) \setminus C\| / (\|V\| - \|C\|)\\, the fraction of
  non-group nodes adjacent to at least one group member. `mode = "in"` /
  `"out"` pick the corresponding directed neighborhood.

## Divergence from NetworkX on betweenness

`networkx.group_betweenness_centrality` uses the Puzis-Yahalom-Elovici
iterative algorithm, which produces results that diverge from the
textbook Everett-Borgatti / Puzis 2008 "at least one node in C"
definition on some graph topologies (verified via an independent Python
brute-force). cograph implements the textbook formula directly;
group_closeness and group_degree match NetworkX exactly.

## References

Everett, M. G., & Borgatti, S. P. (1999). The centrality of groups and
classes. *Journal of Mathematical Sociology*, 23(3), 181-201.

Puzis, R., Yahalom, R., & Elovici, Y. (2008). Augmentative data
collection for betweenness centrality. In *Advances in Social Networks
Analysis and Mining* (pp. 196-200). IEEE.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
per-node measures.

## Examples

``` r
g <- igraph::make_graph("Zachary")
group_centrality(g, nodes = c(1, 2, 3), measure = "betweenness")
#> [1] 0.5754019
group_centrality(g, nodes = c(1, 2, 3), measure = "closeness")
#> [1] 0.7045455
group_centrality(g, nodes = c(1, 2, 3), measure = "degree")
#> [1] 0.6129032
```
