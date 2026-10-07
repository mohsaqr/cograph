# Consensus Community Detection

Runs a stochastic community detection algorithm repeatedly and derives
consensus communities by thresholding the co-occurrence matrix of the
runs.

## Usage

``` r
community_consensus(
  x,
  method = c("louvain", "leiden", "infomap", "label_propagation", "spinglass"),
  n_runs = 100,
  threshold = 0.5,
  seed = NULL,
  ...
)

com_consensus(
  x,
  method = c("louvain", "leiden", "infomap", "label_propagation", "spinglass"),
  n_runs = 100,
  threshold = 0.5,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object. The louvain and leiden methods require an undirected network.

- method:

  Community detection algorithm, one of `"louvain"` (default),
  `"leiden"`, `"infomap"`, `"label_propagation"` or `"spinglass"`. The
  current code runs louvain for `"spinglass"`.

- n_runs:

  Number of runs. Default 100.

- threshold:

  Co-occurrence threshold. Default 0.5. Pairs of nodes that share a
  community in at least this proportion of runs are linked in the
  consensus graph.

- seed:

  Optional seed for reproducibility. If provided, the RNG state is
  initialized once before repeated runs and restored on exit.

- ...:

  Ignored. Each run calls the igraph function with its own defaults and
  without weights or resolution settings. For leiden this is the CPM
  objective with resolution 1.

## Value

A `cograph_communities` data frame (columns `node` and `community`)
holding the consensus membership. Its `"algorithm"` attribute is
`"consensus_<method>"` and its `"modularity"` attribute is the
modularity of the final walktrap partition computed on the consensus
graph.

## Details

The algorithm is run `n_runs` times on the current random number stream.
The proportion of runs in which each pair of nodes shares a community
forms the co-occurrence matrix. Pairs with a proportion of at least
`threshold` are linked in an unweighted consensus graph, and walktrap on
that graph gives the final communities.

## References

Lancichinetti, A., & Fortunato, S. (2012). Consensus clustering in
complex networks. *Scientific Reports*, 2, 336.

## See also

[`communities`](https://sonsoles.me/cograph/reference/communities.md),
[`community_louvain`](https://sonsoles.me/cograph/reference/community_louvain.md)

## Examples

``` r
community_consensus(to_undirected(regulation_net), method = "louvain",
                    n_runs = 10, seed = 1)
#> Community structure (consensus_louvain)
#>   Nodes: 10  | Communities: 2  | Modularity: 0.5 
#>   Sizes: 5, 5 
#> 
#>        node community
#>     Explore         2
#>        Plan         1
#>     Monitor         1
#>       Adapt         2
#>     Reflect         2
#>     Discuss         2
#>  Synthesize         2
#>    Evaluate         1
#>      Create         1
#>       Share         1
```
