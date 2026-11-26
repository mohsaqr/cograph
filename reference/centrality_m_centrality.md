# M-Centrality

M-centrality (Ibnoulouafi et al. 2018) mixes the coreness of a node with
a local variation of degree among its neighbors, by weights taken from
the entropy of the two terms over the network: \$\$M_i = \mu K_s(i) +
(1 - \mu) \Delta D_i, \qquad \Delta D_i = k_i \sum\_{j \in N(i)}
\frac{\|k_j - k_i\|}{\sum\_{l \in N(i)} k_l}, \qquad \mu = \frac{1 -
E_1}{2 - E_1 - E_2},\$\$ with \\k\\ the degree, \\K_s\\ the coreness and
\\E_1\\, \\E_2\\ the entropies of the shares \\K_s(j)/\sum K_s\\ and
\\\Delta D_j / \sum \Delta D\\, divided by \\\ln n\\.

## Usage

``` r
centrality_m_centrality(x, ...)
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
The absolute value covers the whole fraction of the source's equation
(7); the Centrality Zoo prints the term without it, which makes it
negative for hubs. When the weights of equation (11) have no value the
two terms are weighted 1/2 each (owner decision 2026-11-26): on a
regular network, and on any network where every node's neighbors share
its degree, \\\Delta D\\ is 0 at every node and its shares are 0/0; the
same applies when both entropies are 1, on a network without edges and
on a one-node network. With equal weights such a network scores half its
coreness. An isolated node has an empty neighbor sum, \\\Delta D = 0\\,
and coreness 0. The source prints no score; the implementation
reproduces the values and the weight \\\mu = 0.37263\\ that two source
audits computed independently on the Krackhardt kite (10 of 10). On four
test networks no existing measure has a mean Kendall correlation above
0.56 with the score
([`centrality_nwrank`](https://sonsoles.me/cograph/reference/centrality_nwrank.md)
0.555); with the degree the correlation is 0.00 to 0.84 and with the
coreness -0.09 to 0.72. `normalized = TRUE` divides the scores by their
maximum.

## References

Ibnoulouafi, A., El Haziti, M. and Cherifi, H. (2018). M-centrality:
identifying key nodes based on global position and local degree
variation. Journal of Statistical Mechanics: Theory and Experiment,
2018(7), 073407.
[doi:10.1088/1742-5468/aace08](https://doi.org/10.1088/1742-5468/aace08)
.

## See also

[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_m_centrality(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.3703704  1.1250000  2.2702703  1.3548387  0.5769231  0.5357143  1.3333333 
#>   Evaluate     Create      Share 
#>  0.8333333  0.9090909  0.8333333 
```
