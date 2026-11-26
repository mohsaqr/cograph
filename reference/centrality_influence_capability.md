# Influence Capability

Influence capability (IC; Wang et al. 2017) adds a position term from
the k-shell decomposition and a neighborhood term, each divided by its
total and weighted by the entropy weight method: \$\$IC(i) = w_1
\frac{ICP(i)}{\sum_l ICP(l)} + w_2 \frac{ICN(i)}{\sum_l ICN(l)}, \qquad
ICP(i) = k_i - \|\Gamma(i)\| + \sum\_{j \in \Gamma(i)} \tfrac{2}{\pi}
\arctan Iter(j)^{1/3}, \qquad ICN(i) = \sum\_{j \in N(i)} \sum\_{l \in
N(j)} k_l,\$\$ with \\k\\ the degree, \\Iter(j)\\ the iteration of the
k-shell decomposition in which \\j\\ is removed, counted across the
shells from 1, \\\Gamma(i)\\ the neighbors of \\i\\ removed in an
earlier iteration, and \\w_x = (1 - H_x) / (2 - H_1 - H_2)\\, \\H_x\\
the entropy of the shares of attribute \\x\\ divided by \\\ln n\\.

## Usage

``` r
centrality_influence_capability(x, ...)
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

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored. An
iteration removes at once every remaining node whose degree is at most
the current level, which starts at 1; a check that removes nothing
raises the level and does not count. An isolated node is removed in the
first iteration, with the leaves. The source's equation (4) weights
"normalized" attributes without saying how; they are divided by their
totals, the normalisation of its equation (5) (owner decision
2026-11-26). Its equation (7) is printed with a leading minus sign,
which its positive weights rule out. When both attributes take one value
at every node, as on a regular network, equation (7) is 0/0; the weights
are then one half each, which does not change the scores. The
implementation reproduces the iteration numbers, position terms and
neighborhood terms printed in the source's worked example, the 14 rank
groups of the IC column of its Table 2, the ranking of its Table 1 (once
three labels of its Fig. 3, which the table numbers differently, are
exchanged), and the monotonicity of its Table 3 on the dolphins network
(0.9958) and the karate club (0.9542, which is also the largest value an
exact structural measure can reach there). The eight entropy weights the
source prints for these four networks do not follow from its equations
(they differ by 0.003 to 0.042); no printed ranking depends on them. On
a network without edges equation (5) is 0/0, and the measure warns
(class `cograph_undefined_measure`) and returns `NA`. On four test
networks the score ranks nodes closest to
[`centrality_lhc`](https://sonsoles.me/cograph/reference/centrality_lhc.md)
(mean Kendall correlation 0.95) and `eskh` (0.93); with the degree the
correlation is 0.79 to 0.91. `normalized = TRUE` divides the scores by
their maximum.

## References

Wang, Z., Du, C., Fan, J. and Xing, Y. (2017). Ranking influential nodes
in social networks based on node position and neighborhood.
Neurocomputing, 260, 466-477.
[doi:10.1016/j.neucom.2017.04.064](https://doi.org/10.1016/j.neucom.2017.04.064)
.

## See also

[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_ks_if`](https://sonsoles.me/cograph/reference/centrality_ks_if.md),
[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_influence_capability(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.09280429 0.10849957 0.12491403 0.10347724 0.09148043 0.09323724 0.07968034 
#>   Evaluate     Create      Share 
#> 0.09756676 0.11008191 0.09825819 
```
