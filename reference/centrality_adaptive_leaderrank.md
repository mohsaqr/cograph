# Adaptive LeaderRank centrality

Xu and Wang's adaptive LeaderRank computes original node H-indices, then
adds a ground node with H-index one, joined bidirectionally to every
original node. Each augmented arc from j to i has weight \\a\_{ji}h_i\\.
Row-normalized weights define the resource transition matrix. Raw
stationary scores retain total augmented mass N, following initial score
one on ordinary nodes and zero on ground. The ground score is omitted
without redistribution. H-indices are not recomputed after ground edges
are added.

## Usage

``` r
centrality_adaptive_leaderrank(x, alr_h_mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- alr_h_mode:

  Original H-index convention: all (default), out or in.

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in input node order.

## Details

The H-index is the largest integer h for which at least h original
neighbors have degree at least h. The focal node is excluded from that
neighbor list. This differs from cograph's existing closed-neighborhood
[`centrality_lobby`](https://sonsoles.me/cograph/reference/centrality_lobby.md)
convention.

The paper evaluates directed and undirected networks but does not pin a
directed H-index convention. `alr_h_mode` makes that choice explicit.
Default `"all"` computes H-indices on the simple undirected skeleton,
merging reciprocal arcs. `"out"` uses outgoing neighbors' out-degrees;
`"in"` uses incoming neighbors' in-degrees. These directed H-index
choices are explicit cograph conventions, not claims of the authors'
directed-software behavior. In every case, resource flow retains the
original directed arcs. Undirected edges become opposite arcs, and all
H-index modes then coincide.

Input weights are ignored; the algorithm generates its own destination
weights. Loops are removed and parallel arcs count once. The generic
`mode`, inversion and cutoff arguments are ignored. Original nodes with
H-index zero receive zero stationary score. If every H-index is zero,
the ground transition row is undefined and all scores are NaN. This can
occur on edgeless inputs or some directed inputs in in/out H-index
modes. Empty input returns an empty vector. No H-index pseudocount is
added.

A native ground-elimination solve obtains the unique stationary solution
in O(N^3) time and O(N^2) memory, including periodic chains for which
ordinary iteration need not converge. Optional final max normalization
acts on the returned ordinary-node scores. Numerical definition
agreement does not establish superior spreading predictions or
author-code parity.

## References

Xu, S., & Wang, P. (2017). Identifying important nodes by adaptive
LeaderRank. Physica A, 469, 654-664, section 2.2, equation 3 and
algorithm steps 1-4.
[doi:10.1016/j.physa.2016.11.034](https://doi.org/10.1016/j.physa.2016.11.034)
.

## Examples

``` r
centrality_adaptive_leaderrank(igraph::make_ring(4))
#>         1         2         3         4 
#> 0.8333333 0.8333333 0.8333333 0.8333333 
```
