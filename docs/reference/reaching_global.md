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

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"in"`, or `"out"`.

- ...:

  Additional arguments passed to
  [`centrality_reaching_local`](https://sonsoles.me/cograph/reference/centrality_reaching_local.md).

## Value

A single numeric value in \\\[0, 1\]\\.

## Details

Values close to 0 indicate a flat network (all nodes reach equal
proportions of the graph); values close to 1 indicate strong
hierarchical structure. Matches `networkx.global_reaching_centrality`
exactly.

## References

Mones, E., Vicsek, L., & Vicsek, T. (2012). Hierarchy measure for
complex networks. *PLoS ONE*, 7(3), e33799.

## See also

[`centrality_reaching_local`](https://sonsoles.me/cograph/reference/centrality_reaching_local.md),
[`summarize_network`](https://sonsoles.me/cograph/reference/summarize_network.md).

## Examples

``` r
# Star graph: highly hierarchical (directed out from center)
adj <- matrix(0, 5, 5)
adj[1, 2:5] <- 1
rownames(adj) <- colnames(adj) <- LETTERS[1:5]
reaching_global(adj, mode = "out")
#> [1] 1
```
