# Fixed-Radius, Fuzzy and Volume Local Dimensions

Three further members of the local-dimension family, all computed from
hop counts (edge weights are ignored) with the centre node counted in
its own ball, as in
[`centrality_local_dimension`](https://sonsoles.me/cograph/reference/centrality_local_dimension.md).

## Usage

``` r
centrality_local_dimension_fixed(x, mode = "all", ld_radius = 2, ...)

centrality_fuzzy_local_dimension(x, mode = "all", ...)

centrality_local_volume_dimension(x, mode = "all", ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"out"` (distances along
  out-edges), or `"in"`.

- ld_radius:

  Radius \\r\\ for `local_dimension_fixed`. Default 2.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector, one value per node.

## Details

- `local_dimension_fixed` (Silva & Costa 2013):

  The discretised estimator \\D_i(r) = r\\ n_i(r) / B_i(r)\\ at one
  radius `ld_radius` (default 2), where \\n_i(r)\\ is the ring at
  distance \\r\\ and \\B_i(r)\\ the ball within it. A structural
  descriptor rather than an importance ranking; nodes with eccentricity
  below the radius score 0. The paper defines a curve in \\r\\ and fixes
  \\r\\ per figure; the Zoo lists this fixed-radius form separately from
  Pu et al.'s regression form.

- `fuzzy_local_dimension` (Wen & Jiang 2019):

  Fuzzy ball \\N_i(r) = \sum\_{d\_{ij} \le r} e^{-d\_{ij}^2 / r^2} /
  \|\\j : d\_{ij} \le r\\\|\\ for \\r = 1, \ldots, d\_{\max}(i)\\; the
  measure is the slope of \\\log N_i(r)\\ on \\\log r\\. Larger = more
  influential. Reproduces Table 1 of the paper (Krackhardt kite) and its
  karate-club top ten in order.

- `local_volume_dimension` (Li & Deng 2021):

  Volume \\V_i(l) = \sum\_{d\_{ij} \le l} k_j\\, \\l = 1, \ldots,
  ecc(i)\\; the measure is the slope of \\\ln V_i(l)\\ on \\\ln l\\.
  Smaller = more important. The article is closed access; the definition
  follows the authors' own later preprint and the Zoo entry, and no
  published per-node values exist to check against.

The two regression measures return `NaN` for a node with fewer than two
radii.

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
[`centrality_local_information_dimension`](https://sonsoles.me/cograph/reference/centrality_local_information_dimension.md).

## Examples

``` r
path5 <- matrix(0, 5, 5)
path5[cbind(1:4, 2:5)] <- 1; path5 <- path5 + t(path5)
rownames(path5) <- colnames(path5) <- LETTERS[1:5]
centrality_local_dimension_fixed(path5)
#>         A         B         C         D         E 
#> 0.6666667 0.5000000 0.8000000 0.5000000 0.6666667 
centrality_fuzzy_local_dimension(path5)
#>          A          B          C          D          E 
#> 0.04895549 0.25765248 0.18702827 0.25765248 0.04895549 
centrality_local_volume_dimension(path5)
#>         A         B         C         D         E 
#> 0.7252466 0.4340194 0.4150375 0.4340194 0.7252466 
```
