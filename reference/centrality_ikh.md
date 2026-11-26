# Improved K-Shell Hybrid Method

The improved k-shell hybrid method (IKH; Maji et al. 2020) replaces the
sum of the k-shell indices in
[`centrality_ksh`](https://sonsoles.me/cograph/reference/centrality_ksh.md)
by their product, and its constant weight of the degree by a weight that
falls with the distance: \$\$IKH(i) = \sum\_{j:\\1 \le d\_{ij} \le r}
\frac{\sqrt{ks_i\\ks_j} + \mu(d\_{ij})\\k_j}{d\_{ij}^2}, \qquad \mu(l) =
\frac{2(r - l + 1)}{r(r + 1)}.\$\$ The weights \\\mu(1), \dots, \mu(r)\\
sum to 1, so the measure has no free weight.

## Usage

``` r
centrality_ikh(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `ksh_radius` (the largest distance summed over,
  default 3).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The radius is `ksh_radius`, 3 by default as in all experiments of the
source. It must be finite, because the weights are defined over a finite
number of levels. The source asks for a radius no larger than the
diameter; a larger one is accepted, and the weights of the levels that
exist then sum to less than 1. The shell term is the root of the
product: the equation, the text, the worked example and the table of the
source all use it. The Centrality Zoo prints the root of the sum, taken
over from ksh, which reproduces none of the printed values. The
implementation reproduces the 13 values of the source's Table 1 and its
worked value, on the 13-node network of Namtirtha et al. (2018).
Isolated nodes score 0. A `ksh_radius` that is not a finite whole number
of at least one raises a `cograph_bad_parameter` error.
`normalized = TRUE` divides the scores by their maximum.

## References

Maji, G., Namtirtha, A., Dutta, A. and Curado Malta, M. (2020).
Influential spreaders identification in complex networks with improved
k-shell hybrid method. Expert Systems with Applications, 144, 113092.
[doi:10.1016/j.eswa.2019.113092](https://doi.org/10.1016/j.eswa.2019.113092)
.

## See also

[`centrality_ksh`](https://sonsoles.me/cograph/reference/centrality_ksh.md),
[`centrality_eikh`](https://sonsoles.me/cograph/reference/centrality_eikh.md),
[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_ikh(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   39.33333   44.33333   49.33333   43.91667   38.91667   39.75000   35.16667 
#>   Evaluate     Create      Share 
#>   40.58333   44.75000   40.58333 
centrality_ikh(regulation_net, ksh_radius = 2)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   43.83333   49.66667   55.50000   49.08333   43.25000   44.41667   39.16667 
#>   Evaluate     Create      Share 
#>   45.58333   50.25000   45.58333 
```
