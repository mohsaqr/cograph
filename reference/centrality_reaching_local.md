# Local Reaching Centrality (Mones, Vicsek & Vicsek 2012)

Local reaching centrality measures how much of the network is reachable
from a node.

## Usage

``` r
centrality_reaching_local(x, mode = "all", ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"in"`, or `"out"`.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of local reaching centrality values.

## Details

- Directed unweighted: \\LRC(v) = \|\\u : u \ne v, v \to u\\\| / (N -
  1)\\.

- Undirected unweighted: average of \\1/d(v, u)\\ over all \\u \ne v\\,
  divided by \\N - 1\\. Numerically equal to
  `igraph::harmonic_centrality(normalized = TRUE)`.

- Weighted: NetworkX convention, where edge weights are interpreted as
  strengths and path length is \\\sum_e (\mathrm{total\\weight} /
  w_e)\\. Per-path score is the mean of original edge weights along the
  shortest path.

Bit-exact match against `networkx.local_reaching_centrality` across all
three branches. Bit-exact match against
`igraph::harmonic_centrality(normalized = TRUE)` for the undirected
unweighted branch. See
[`reaching_global`](https://sonsoles.me/cograph/reference/reaching_global.md)
for the graph-level hierarchy measure derived from per-node LRC.

## References

Mones, E., Vicsek, L., & Vicsek, T. (2012). Hierarchy measure for
complex networks. *PLoS ONE*, 7(3), e33799.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md),
[`reaching_global`](https://sonsoles.me/cograph/reference/reaching_global.md).

## Examples

``` r
# Directed path A -> B -> C
adj <- matrix(c(0,1,0, 0,0,1, 0,0,0), 3, 3, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_reaching_local(adj, mode = "out")
#>   A   B   C 
#> 1.0 0.5 0.0 
```
