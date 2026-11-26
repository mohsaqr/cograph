# Degree and Clustering Coefficient Centrality

Degree and clustering coefficient centrality (DCC; Yang et al. 2020)
adds a degree term and a clustering term, weighted by the entropy weight
method: \$\$DCC(i) = \alpha I_D(i) + \beta I_C(i), \qquad I_D(i) = k_i +
\sum\_{j \in N(i)} k_j, \qquad I_C(i) = e^{-C_i} \sum\_{j \in N_2(i)}
C_j,\$\$ with \\k\\ the degree, \\C\\ the local clustering coefficient
and \\N_2(i)\\ the nodes within two steps of \\i\\. Each attribute is
divided by its Euclidean norm over the nodes, \\E_1\\ and \\E_2\\ are
the entropies of the results divided by \\\ln n\\, and \\\alpha = (1 -
E_1) / (2 - E_1 - E_2)\\, \\\beta = 1 - \alpha\\.

## Usage

``` r
centrality_dcc(x, ...)
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
\\N_2(i)\\ holds the nodes one or two steps away: the clustering sum the
source prints for node 8 of its Fig. 1(b), and its karate top five, need
that reading; the Centrality Zoo says exactly two steps. The clustering
coefficient is 0 below degree 2. The weights are computed for each
network, so adding a component changes every score. On a network without
a triangle the clustering term is 0 at every node and the entropy
weights are 0/0; the measure then warns (class
`cograph_undefined_measure`) and returns `NA`. It does the same when the
two entropies add up to 2, where the weights divide by zero. The
implementation reproduces the karate top five of the source's Table 3
and the share of tied scores of its Table 2 (32.35 percent). The
source's worked example on its Fig. 1(b) prints 23 numbers: the 14
attribute values among them follow from the network drawn there, but its
normalised attributes, its entropy, its weight \\\alpha\\ and its score
of node 1 (16.2677) do not. The printed entropy weights cannot be
reproduced from the printed network; the score of node 1 here is
15.7429. On four test networks the score ranks nodes closest to
`diffusion` and the two-step expected influence (mean Kendall
correlation 0.94) and to
[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md)
(0.88 to 0.96). `normalized = TRUE` divides the scores by their maximum.

## References

Yang, Y., Wang, X., Chen, Y., Hu, M. and Ruan, C. (2020). A novel
centrality of influential nodes identification in complex networks. IEEE
Access, 8, 58742-58751.
[doi:10.1109/ACCESS.2020.2983053](https://doi.org/10.1109/ACCESS.2020.2983053)
.

## See also

[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md),
[`centrality_transitivity`](https://sonsoles.me/cograph/reference/centrality_transitivity.md),
[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_dcc(to_undirected(regulation_net))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   17.23582   20.28402   23.18891   20.02044   17.09047   17.90136   15.23920 
#>   Evaluate     Create      Share 
#>   18.73328   20.67871   18.58562 
```
