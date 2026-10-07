# Weighted k-shell, Renewed Coreness and Geodesic k-path

Three shell and path counts. The weighted k-shell (Garas, Schweitzer and
Havlin 2012) runs the k-shell decomposition on the generalized degree
\$\$k'\_i = \left(k_i^{\alpha} s_i^{\beta}\right)^{1 / (\alpha +
\beta)},\$\$ with strength \\s_i\\ after the weight normalization of the
source. Renewed coreness (Liu, Tang, Zhou and Do 2015) gives each link
the diffusion importance \\D\_{ij} = (\|N(j) \setminus N\[i\]\| + \|N(i)
\setminus N\[j\]\|) / 2\\, removes the links below `renewed_threshold`,
and returns the k-core number of the residual graph. Geodesic k-path
centrality (Borgatti and Everett 2006) counts the shortest paths of
length at most `kpath_k` that start at the node, with multiplicity.

## Usage

``` r
centrality_weighted_kshell(x, wks_alpha = 1, wks_beta = 1, ...)

centrality_renewed_coreness(x, renewed_threshold = 2, ...)

centrality_geodesic_kpath(x, mode = "all", kpath_k = 3, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- wks_alpha, wks_beta:

  Exponents of degree and strength in the weighted k-shell (default 1
  and 1).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The weighted k-shell uses `weighted` (use edge weights, default
  `TRUE`).

- renewed_threshold:

  Diffusion-importance threshold for renewed coreness (default 2).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- kpath_k:

  Maximum path length for geodesic k-path (default 3).

## Value

A named numeric vector with one score per node, in input node order. The
weighted k-shell and renewed coreness are integer vectors.

## Details

The weighted k-shell uses edge weights, and with unit weights it equals
the k-core number. Isolated nodes score 0. Renewed coreness ignores
weights, and a clique with no outside links scores 0. Both ignore
direction. Geodesic k-path ignores weights and follows `mode`. The
Centrality Zoo transcribes the diffusion importance with open
neighborhoods, which is off by one, and the closed neighborhoods of the
source are used here.
[`centiserve::geokpath`](https://rdrr.io/pkg/centiserve/man/geokpath.html)
counts the nodes within \\k\\ hops, which is the vertex-disjoint variant
of Borgatti and Everett.

## References

Garas, A., Schweitzer, F., & Havlin, S. (2012). A k-shell decomposition
method for weighted networks. New Journal of Physics, 14, 083030.

Liu, Y., Tang, M., Zhou, T., & Do, Y. (2015). Improving the accuracy of
the k-shell method by removing redundant links: From a perspective of
spreading dynamics. Scientific Reports, 5, 13172.
[doi:10.1038/srep13172](https://doi.org/10.1038/srep13172) .

Borgatti, S. P., & Everett, M. G. (2006). A graph-theoretic perspective
on centrality. Social Networks, 28(4), 466-484.

## See also

[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_s_shell`](https://sonsoles.me/cograph/reference/centrality_s_shell.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_weighted_kshell(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          8          8          8          8          8          8          7 
#>   Evaluate     Create      Share 
#>          8          8          8 
centrality_renewed_coreness(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          3          3          3          3          3          3          3 
#>   Evaluate     Create      Share 
#>          3          3          3 
centrality_geodesic_kpath(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>         17         18         15         21         20         20         18 
#>   Evaluate     Create      Share 
#>         20         17         18 
```
