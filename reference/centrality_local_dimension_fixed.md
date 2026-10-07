# Fixed-Radius, Fuzzy and Volume Local Dimensions

Three members of the local-dimension family, with the center node
counted in its own ball. The fixed-radius local dimension (Silva and
Costa 2013) is \\D_i(r) = r\\ n_i(r) / B_i(r)\\ at one radius \\r\\,
where \\n_i(r)\\ counts the nodes at distance \\r\\ and \\B_i(r)\\ those
within it. The fuzzy local dimension (Wen and Jiang 2019) is the slope
of \\\log N_i(r)\\ on \\\log r\\ for the fuzzy ball \$\$N_i(r) =
\frac{\sum\_{d\_{ij} \le r} e^{-d\_{ij}^2 / r^2}} {\|\\j : d\_{ij} \le
r\\\|}.\$\$ The local volume dimension (Li and Deng 2021) is the slope
of \\\ln V_i(l)\\ on \\\ln l\\ for the volume \\V_i(l) = \sum\_{d\_{ij}
\le l} k_j\\.

## Usage

``` r
centrality_local_dimension_fixed(x, mode = "all", ld_radius = 2, ...)

centrality_fuzzy_local_dimension(x, mode = "all", ...)

centrality_local_volume_dimension(x, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- ld_radius:

  Radius \\r\\ for `local_dimension_fixed`, in hops (default 2).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Distances are hop counts, so edge weights are ignored. On a directed
network `mode` sets the direction of the paths. The fixed-radius form is
a structural descriptor, and nodes whose eccentricity is below the
radius score 0. Larger fuzzy dimensions and smaller volume dimensions
mark more influential nodes. The two regression forms return `NaN` for a
node with fewer than two radii. The volume dimension follows a later
preprint of the authors and the Centrality Zoo entry. A `ld_radius` that
is not a single number of at least 1 raises an error of class
`cograph_bad_parameter`.

## References

Silva, F. N., & Costa, L. da F. (2013). Local dimension of complex
networks. arXiv:1209.2476.

Wen, T., & Jiang, W. (2019). Identifying influential nodes based on
fuzzy local dimension in complex networks. Chaos, Solitons & Fractals,
119, 332-342.

Li, H., & Deng, Y. (2021). Local volume dimension: A novel approach for
important nodes identification in complex networks. International
Journal of Modern Physics B, 35(5), 2150069.

## See also

[`centrality_local_dimension`](https://sonsoles.me/cograph/reference/centrality_local_dimension.md),
[`centrality_local_information_dimension`](https://sonsoles.me/cograph/reference/centrality_local_information_dimension.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_local_dimension_fixed(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        0.8        0.6        0.4        0.6        0.8        0.8        1.0 
#>   Evaluate     Create      Share 
#>        0.8        0.6        0.8 
centrality_fuzzy_local_dimension(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.4277285  0.5646062  0.6855285  0.5646062  0.4277285  0.4277285  0.2686074 
#>   Evaluate     Create      Share 
#>  0.4277285  0.5646062  0.4277285 
centrality_local_volume_dimension(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.7548875  0.5069600  0.2954559  0.5454341  0.8006912  0.7104934  0.9475326 
#>   Evaluate     Create      Share 
#>  0.6256045  0.4694853  0.6256045 
```
