# Mixed Core, Degree and Entropy

Mixed core, degree and entropy (MCDE; Sheikhahmadi and Nematbakhsh 2017)
adds the k-shell index of a node, its degree and the entropy of the
shells its neighbors lie in: \$\$MCDE(v) = ks(v) + k_v + H(v), \qquad
H(v) = -\sum_i p_i(v) \log_2 p_i(v),\$\$ where \\p_i(v)\\ is the share
of the neighbors of \\v\\ in shell \\i\\.

## Usage

``` r
centrality_mcde(x, ...)
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
network, so direction, weights, loops and parallel edges are ignored.
The three terms have weight 1 and are added as they are, as in the
source's experiments and in its worked table. The logarithm is to base
2, as printed. The degree is the largest term, so the score ranks nodes
much like the degree (Kendall correlation 0.97 on four test networks);
on a network with a single shell the entropy is 0 and the ranking is
that of the degree. The implementation reproduces 22 of the 23 scores of
the source's Table 2 to one decimal and its rank order, for its 23-node
example on the edge list read from its figure. The source prints node 4
as 11.5, where the exact value is 11.449: its entropy was rounded twice.
Isolated nodes score 0. `normalized = TRUE` divides the scores by their
maximum.

## References

Sheikhahmadi, A. and Nematbakhsh, M. A. (2017). Identification of
multi-spreader users in social networks for viral marketing. Journal of
Information Science, 43(3), 412-423.
[doi:10.1177/0165551516644171](https://doi.org/10.1177/0165551516644171)
.

## See also

[`centrality_mcdwe`](https://sonsoles.me/cograph/reference/centrality_mcdwe.md),
[`centrality_mcsde`](https://sonsoles.me/cograph/reference/centrality_mcsde.md),
[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_mcde(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          9         10         11         10          9          9          8 
#>   Evaluate     Create      Share 
#>          9         10          9 
```
