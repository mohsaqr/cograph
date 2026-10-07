# Hybrid Global Structure Model Centrality

The hybrid global structure model (H-GSM; Mukhtar et al. 2023) combines
a self-influence \\s_i = \exp(k_s(i)\\ k_i / N)\\ with a distance
exponent \\a\\ computed from the mean self-influence: \$\$HGSM(i) = s_i
\sum\_{j \ne i} \frac{s_j}{d\_{ij}^{a}}, \qquad a = \left\lceil \log_2
\frac{1}{N} \sum_l s_l \right\rceil.\$\$ Here \\k_s(i)\\ is the core
number, \\k_i\\ the degree and \\N\\ the number of nodes, isolated nodes
included.

## Usage

``` r
centrality_hybrid_global_structure(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure uses the simple undirected skeleton, so direction, weights,
loops and parallel edges are ignored, and `mode` has no effect. Only
reachable nodes contribute to the sum. Other components affect the
scores through \\N\\ and the mean self-influence, to which each isolated
node adds 1. An isolated node scores 0. Raw scores beyond double
precision raise an error. With `normalized = TRUE` the scores are
computed on the log scale and remain available in that case.

## References

Mukhtar, M. F., et al. (2023). Integrating local and global information
to identify influential nodes in complex networks. Scientific Reports,
13, 11411.
[doi:10.1038/s41598-023-37570-7](https://doi.org/10.1038/s41598-023-37570-7)
.

## See also

[`centrality_global_structure`](https://sonsoles.me/cograph/reference/centrality_global_structure.md),
[`centrality_improved_global_structure`](https://sonsoles.me/cograph/reference/centrality_improved_global_structure.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_hybrid_global_structure(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   345.0810   619.5092  1004.9876   581.9533   340.5875   370.2555   239.8073 
#>   Evaluate     Create      Share 
#>   432.9857   644.6836   432.9857 
```
