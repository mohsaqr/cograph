# Extended Cluster Coefficient Ranking Measure

The extended cluster coefficient ranking measure (ECRM; Zareie et al.
2020) adds the shell clustering coefficient over the walks of two steps
from a node: \$\$ECRM_i = \sum\_{j \in N(i)} CRM_j, \qquad CRM_j =
\sum\_{l \in N(j)} SCC_l,\$\$ with \\SCC\\ the shell clustering
coefficient of
[`centrality_shell_clustering`](https://sonsoles.me/cograph/reference/centrality_shell_clustering.md).

## Usage

``` r
centrality_ecrm(x, ...)
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
These are equations (5) and (6) of the source and lines 15 to 26 of its
Algorithm 1; the node itself is reached once for each of its neighbors.
The Centrality Zoo's entry for ECRM gives no source; it is this one. The
conventions are those of
[`centrality_shell_clustering`](https://sonsoles.me/cograph/reference/centrality_shell_clustering.md):
hierarchies by the lowest-degree peeling, and a correlation that is 0/0
taken as 0. An isolated node scores 0. The source prints no score. The
implementation reproduces the monotonicity of the source's Table 2 on
the dolphins network (0.9979) and on the adjective-noun network of David
Copperfield (0.9997); each of these is also the largest value an exact
structural measure can reach on its network, so they show only that the
measure separates every pair of nodes the network does not make
equivalent. On four test networks the score ranks nodes closest to
[`centrality_diffusion_centrality`](https://sonsoles.me/cograph/reference/centrality_diffusion_centrality.md)
(mean Kendall correlation 0.967) and
[`centrality_ninl`](https://sonsoles.me/cograph/reference/centrality_ninl.md)
(0.964); with
[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md)
the correlation is 0.93 to 0.95. `normalized = TRUE` divides the scores
by their maximum.

## References

Zareie, A., Sheikhahmadi, A., Jalili, M. and Fasaei, M. S. K. (2020).
Finding influential nodes in social networks based on neighborhood
correlation coefficient. Knowledge-Based Systems, 194, 105580.
[doi:10.1016/j.knosys.2020.105580](https://doi.org/10.1016/j.knosys.2020.105580)
.

## See also

[`centrality_shell_clustering`](https://sonsoles.me/cograph/reference/centrality_shell_clustering.md),
[`centrality_erm`](https://sonsoles.me/cograph/reference/centrality_erm.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_ecrm(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   604.8778   747.3049   843.3570   727.0481   607.3261   613.8195   524.2460 
#>   Evaluate     Create      Share 
#>   657.2708   759.1555   661.7483 
```
