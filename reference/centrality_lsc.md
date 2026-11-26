# Local Structural Centrality

Local structural centrality (LSC; Gao et al. 2014) sums, over the
neighbors of a node, the size of each neighbor's two-step neighborhood
and the clustering inside it: \$\$LSC(v) = \sum\_{u \in N(v)}
\Bigl(\alpha\\\|N_2(u)\| + (1 - \alpha) \sum\_{w \in N_2(u)}
c_w\Bigr),\$\$ where \\N_2(u)\\ holds the nodes one or two steps from
\\u\\ and \\c_w\\ is the local clustering coefficient of \\w\\.

## Usage

``` r
centrality_lsc(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `lsc_alpha` (the weight of the neighborhood size
  against the clustering sum, default 0.2).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
`lsc_alpha` is 0.2 by default, the value the source fixes for all its
experiments on real networks; its worked example uses 0.7. With
`lsc_alpha = 1` the clustering term drops out. The two-step neighborhood
does not contain its center, and a node with fewer than two neighbors
has clustering 0; both are what the values of the source require.
Isolated nodes score 0. The implementation reproduces the ten values the
source prints for its 28-node example at `lsc_alpha = 0.7`, on an edge
list recovered from the other columns of the same table. A `lsc_alpha`
outside \\\[0, 1\]\\ raises a `cograph_bad_parameter` error.
`normalized = TRUE` divides the scores by their maximum.

## References

Gao, S., Ma, J., Chen, Z., Wang, G. and Xing, C. (2014). Ranking the
spreading ability of nodes in complex networks based on local structure.
Physica A: Statistical Mechanics and its Applications, 403, 130-147.
[doi:10.1016/j.physa.2014.02.032](https://doi.org/10.1016/j.physa.2014.02.032)
.

## See also

[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md),
[`centrality_transitivity`](https://sonsoles.me/cograph/reference/centrality_transitivity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_lsc(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   25.89524   30.70857   36.09333   30.73524   25.68952   25.92190   20.80381 
#>   Evaluate     Create      Share 
#>   25.90286   30.76190   25.74286 
centrality_lsc(regulation_net, lsc_alpha = 0.7)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   37.83571   45.26571   52.91000   45.27571   37.75857   37.84571   30.30143 
#>   Evaluate     Create      Share 
#>   37.83857   45.28571   37.77857 
```
