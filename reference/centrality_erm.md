# Entropy-Based Ranking Measure

The entropy-based ranking measure (ERM; Zareie et al. 2017) gives each
node an entropy centrality from the spread of its neighbors' degrees and
of their degree sums, and adds it over the walks of two steps:
\$\$ERM(i) = \sum\_{j \in N(i)} \sum\_{l \in N(j)} EC(l), \qquad EC(i) =
E_1(i) + \lambda_i E_2(i), \qquad \lambda_i = \frac{k^{(2)}\_i}{\max_l
k^{(2)}\_l},\$\$ with \\E_1(i)\\ the entropy of the shares \\k_j /
k^{(1)}\_i\\ over the neighbors \\j\\ of \\i\\, \\E_2(i)\\ that of the
shares \\k^{(1)}\_j / k^{(2)}\_i\\, \\k\\ the degree, \\k^{(1)}\_i\\ the
sum of the neighbors' degrees and \\k^{(2)}\_i\\ the sum of the
neighbors' \\k^{(1)}\\.

## Usage

``` r
centrality_erm(x, ...)
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
The source's equation (4) is printed with \\v_j\\ on its right side; the
sentence that introduces it, and the same authors' restatement (Zareie
and Sheikhahmadi 2019, equation 9), have \\v_i\\. The source gives no
base for the logarithm; the natural logarithm is used (owner decision
2026-11-26), and another base multiplies every score by one constant. A
leaf has \\EC = 0\\; a node scores through the nodes its walks of two
steps reach, so a leaf of a star scores 0, and so does an isolated node.
The source prints no score. The implementation reproduces the share of
distinct ranks and the monotonicity of its 21-node example (0.8095 and
0.9623) and of the dolphins network (0.9677 and 0.9979), and the
monotonicity Zareie et al. (2020) print for the adjective-noun network
(0.9997); each of these is also the largest value an exact structural
measure can reach on its network, so they show only that the measure
separates every pair of nodes the network does not make equivalent. On
the karate club the source prints 0.8529 and 0.9612, above the largest
values an exact structural measure can reach there (0.7941 and 0.9542,
the values of the implementation). On four test networks the score ranks
nodes closest to
[`centrality_ninl`](https://sonsoles.me/cograph/reference/centrality_ninl.md)
(mean Kendall correlation 0.984) and
[`centrality_diffusion_centrality`](https://sonsoles.me/cograph/reference/centrality_diffusion_centrality.md)
(0.978); with
[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md)
the correlation is 0.92 to 0.99. `normalized = TRUE` divides the scores
by their maximum.

## References

Zareie, A., Sheikhahmadi, A. and Fatemi, A. (2017). Influential nodes
ranking in complex networks: an entropy-based approach. Chaos, Solitons
and Fractals, 104, 485-494.
[doi:10.1016/j.chaos.2017.09.010](https://doi.org/10.1016/j.chaos.2017.09.010)
.

Zareie, A., Sheikhahmadi, A., Jalili, M. and Fasaei, M. S. K. (2020).
Finding influential nodes in social networks based on neighborhood
correlation coefficient. Knowledge-Based Systems, 194, 105580.
[doi:10.1016/j.knosys.2020.105580](https://doi.org/10.1016/j.knosys.2020.105580)
.

## See also

[`centrality_ehc`](https://sonsoles.me/cograph/reference/centrality_ehc.md),
[`centrality_ecrm`](https://sonsoles.me/cograph/reference/centrality_ecrm.md),
[`centrality_entropy`](https://sonsoles.me/cograph/reference/centrality_entropy.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_erm(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   82.41142  101.18502  114.72021   97.75951   81.09264   83.18577   70.38873 
#>   Evaluate     Create      Share 
#>   88.83466  102.44021   89.38820 
```
