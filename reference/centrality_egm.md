# Effective Gravity Model

The effective gravity model (EGM; Li and Xiao 2021) is a gravity model
without a free radius. Each node has a value \\V_i = I_i k_i\\, its
degree times the entropy of the degrees in its closed neighborhood, and
its own radius of influence \\R_i\\: \$\$EGM(i) = \sum\_{j \ne
i:\\d\_{ij} \le R_i} \frac{V_i V_j}{d\_{ij}^2}, \qquad R_i =
\frac{ecc_i}{1 + \sqrt{\bar{k}\_{far} / k_i}},\$\$ where \\ecc_i\\ is
the eccentricity of \\i\\ and \\\bar{k}\_{far}\\ the mean degree of the
nodes at that distance.

## Usage

``` r
centrality_egm(x, ...)
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
The entropy is \\I_i = -\sum_j p_j \log p_j\\ over the node and its
neighbors, with \\p_j\\ the degree of \\j\\ as a share of their degrees.
The natural logarithm is used; the source prints no base, and another
base multiplies every score by one constant. The radius is a real number
and is not rounded, so a node whose radius is below 1 scores 0 although
it has neighbors. The measure has no value on a disconnected network,
where the eccentricity is infinite, or on a one-node network; there it
warns (class `cograph_undefined_measure`) and returns `NA`. The source
prints no value of the measure, so the implementation is checked against
the definition only. `normalized = TRUE` divides the scores by their
maximum.

## References

Li, S. and Xiao, F. (2021). The identification of crucial spreaders in
complex networks by effective gravity model. Information Sciences, 578,
725-749.
[doi:10.1016/j.ins.2021.08.026](https://doi.org/10.1016/j.ins.2021.08.026)
.

## See also

[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md),
[`centrality_ksgc`](https://sonsoles.me/cograph/reference/centrality_ksgc.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_egm(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>     0.0000   686.8592   982.6602   655.8222     0.0000     0.0000     0.0000 
#>   Evaluate     Create      Share 
#>   519.1044   718.3250   519.5757 
```
