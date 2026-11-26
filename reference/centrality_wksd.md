# Weighted K-Shell Degree Neighborhood

The weighted k-shell degree neighborhood (Wksd; Namtirtha et al. 2018,
2020) weights each edge by the product of a mix of degree and k-shell
index at its two ends, and scores a node by the sum of the weights of
its edges: \$\$Wksd(i) = \sum\_{j \in N(i)} (\alpha k_i + \mu k_s(i))
(\alpha k_j + \mu k_s(j)),\$\$ with \\k\\ the degree and \\k_s\\ the
k-shell index.

## Usage

``` r
centrality_wksd(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `wksd_alpha` and `wksd_mu` (both required).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The two original papers were not read (they are not open access); the
formula is the one three later papers state alike, two of them read from
the page (Shetty and Bhattacharjee 2025, equations 3-4; Lu and Dong
2020, equations 2.5-2.6). They stop at the single sum above; a second
neighborhood sum in the originals cannot be ruled out. `wksd_alpha` and
`wksd_mu` are \\\alpha\\ and \\\mu\\. The source tunes them per network,
and the papers that restate it use different pairs (Lu and Dong 0.9 and
0.2 on five of eight networks, 0.2 and 0.9 on one), so they have no
default: without both the measure warns (class
`cograph_undefined_measure`) and returns `NA` (owner decision
2026-11-26). Each must lie in \\\[0, 1\]\\, the range those papers give,
or a `cograph_bad_parameter` error is raised. With \\\mu = 0\\ the score
is \\\alpha^2 k_i \sum\_{j \in N(i)} k_j\\. An isolated node scores 0.
[`centrality_wksdn`](https://sonsoles.me/cograph/reference/centrality_wksdn.md)
is Maji et al.'s additive variant of the same idea, without parameters.
No paper read prints a score; the implementation reproduces the 35
values a source audit computed on two small networks at three settings.
On four test networks, with `wksd_alpha = 0.2` and `wksd_mu = 0.9` the
score ranks nodes closest to
[`centrality_wksdn`](https://sonsoles.me/cograph/reference/centrality_wksdn.md)
(mean Kendall correlation 0.972) and
[`centrality_lnc`](https://sonsoles.me/cograph/reference/centrality_lnc.md)
(0.970), with correlations of 0.99 or more on single networks: 0.998
with `wksdn` and with `lnc`, 0.993 with
[`centrality_gls`](https://sonsoles.me/cograph/reference/centrality_gls.md),
0.990 with
[`centrality_laplacian`](https://sonsoles.me/cograph/reference/centrality_laplacian.md)
and with
[`centrality_modified_expected_force`](https://sonsoles.me/cograph/reference/centrality_modified_expected_force.md).
With `wksd_alpha = 0.9` and `wksd_mu = 0.2` it ranks them closest to
`laplacian` (0.974),
[`centrality_ked`](https://sonsoles.me/cograph/reference/centrality_ked.md)
(0.968) and `wksdn` (0.964), with 0.999 with
[`centrality_ksgc`](https://sonsoles.me/cograph/reference/centrality_ksgc.md)
on a network with a single k-shell and 0.998 with `laplacian`, 0.996
with `lnc` and 0.992 with `ked` and `wksdn` on another.
`normalized = TRUE` divides the scores by their maximum.

## References

Namtirtha, A., Dutta, A. and Dutta, B. (2018). Weighted kshell degree
neighborhood method: an approach independent of completeness of global
network structure for identifying the influential spreaders. In 2018
10th International Conference on Communication Systems and Networks
(COMSNETS), 81-88.
[doi:10.1109/COMSNETS.2018.8328183](https://doi.org/10.1109/COMSNETS.2018.8328183)
.

Namtirtha, A., Dutta, A. and Dutta, B. (2020). Weighted kshell degree
neighborhood: a new method for identifying the influential spreaders
from a variety of complex network connectivity structures. Expert
Systems with Applications, 139, 112859.
[doi:10.1016/j.eswa.2019.112859](https://doi.org/10.1016/j.eswa.2019.112859)
.

Shetty, R. D. and Bhattacharjee, S. (2025). Pertinence of contact
duration as edge feature for epidemic spread analysis. Scientific
Reports, 15, 10703.
[doi:10.1038/s41598-025-94637-3](https://doi.org/10.1038/s41598-025-94637-3)
.

Lu, P. and Dong, C. (2020). EMH: extended mixing H-index centrality for
identification important users in social networks based on neighborhood
diversity. arXiv:2003.03585.

## See also

[`centrality_wksdn`](https://sonsoles.me/cograph/reference/centrality_wksdn.md),
[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_wksd(regulation_net, wksd_alpha = 0.2, wksd_mu = 0.9)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>     107.64     134.40     163.00     133.44     106.72     108.56      84.48 
#>   Evaluate     Create      Share 
#>     110.40     135.36     110.40 
centrality_wksd(regulation_net, wksd_alpha = 0.9, wksd_mu = 0.2)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>     149.99     208.32     276.19     202.74     145.22     154.76     109.12 
#>   Evaluate     Create      Share 
#>     164.30     213.90     164.30 
```
