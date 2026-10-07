# Expected Centrality

Expected centrality is the sum of the degrees of a node's neighbors:
\$\$E(v) = \sum\_{u \in N(v)} k_u.\$\$

## Usage

``` r
centrality_expected(x, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are ignored. `mode` sets both the degrees and the neighbor
set. On a directed network `mode = "all"` uses total degrees and the
undirected neighbor set. Adding the node's own degree gives the
`"kandhway_kuri"` form of
[`centrality_diffusion`](https://sonsoles.me/cograph/reference/centrality_diffusion.md).

## See also

[`centrality_diffusion`](https://sonsoles.me/cograph/reference/centrality_diffusion.md),
[`centrality_neighborhood_connectivity`](https://sonsoles.me/cograph/reference/centrality_neighborhood_connectivity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_expected(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>         30         35         41         34         28         32         27 
#>   Evaluate     Create      Share 
#>         34         37         34 
```
