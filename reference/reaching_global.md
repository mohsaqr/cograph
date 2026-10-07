# Global Reaching Centrality (Mones, Vicsek & Vicsek 2012)

A graph-level hierarchy measure computed from per-node local reaching
centralities: \$\$GRC(G) = \frac{1}{N - 1} \sum_v \left( \max_u LRC(u) -
LRC(v) \right)\$\$

## Usage

``` r
reaching_global(x, mode = "all", ...)
```

## Arguments

- x:

  Network input (matrix, edge-list data frame, igraph, network,
  cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"in"`, or `"out"`.

- ...:

  Additional arguments passed to
  [`centrality_reaching_local`](https://sonsoles.me/cograph/reference/centrality_reaching_local.md).

## Value

A single numeric value. On an unweighted graph it lies in \\\[0, 1\]\\.
On a weighted graph the local reaching centralities scale with the edge
weights, so the value is unbounded. A graph with at most one node
returns 0.

## Details

Values close to 0 indicate a flat network in which all nodes reach equal
proportions of the graph. Larger values indicate a more hierarchical
structure. The result matches `networkx.global_reaching_centrality`.

## References

Mones, E., Vicsek, L., & Vicsek, T. (2012). Hierarchy measure for
complex networks. *PLoS ONE*, 7(3), e33799.

## See also

[`centrality_reaching_local`](https://sonsoles.me/cograph/reference/centrality_reaching_local.md),
[`summarize_network`](https://sonsoles.me/cograph/reference/summarize_network.md).

## Examples

``` r
reaching_global(regulation_net, mode = "out")
#> [1] 0.08522634
```
