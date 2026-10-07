# Diffusion Centrality

Diffusion centrality has two forms, chosen by `diffusion_method`. The
`"kandhway_kuri"` form (Kandhway and Kuri 2014) adds the degrees of a
node's neighbors to its own degree, both scaled by \\\lambda\\: \\DC(v)
= \lambda k_v + \lambda \sum\_{u \in N(v)} k_u\\. The `"power_series"`
form sums the rows of the first \\n\\ powers of the weight matrix:
\$\$DC(v) = \sum\_{w} \left( W + W^2 + \cdots + W^n \right)\_{vw}.\$\$

## Usage

``` r
centrality_diffusion(x, mode = "all", lambda = 1, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- lambda:

  Scale factor \\\lambda\\ of the `"kandhway_kuri"` form. Default 1.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `diffusion_method` (`"kandhway_kuri"` or
  `"power_series"`, default `NULL`, which picks by input type) and
  `loops` (default `TRUE`, `FALSE` for tna input).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The default is `"kandhway_kuri"`, and `"power_series"` for tna input.
The `"kandhway_kuri"` form uses binary degrees, so edge weights are
ignored, and `mode` sets both the degrees and the neighbor set. On a
directed network `mode = "all"` uses total degrees and the undirected
neighbor set. `lambda` multiplies every score. The `"power_series"` form
uses the edge weights and ignores `mode`, `lambda` and `weighted`. With
`loops = FALSE` the diagonal of \\W\\ is set to zero. The
`"power_series"` values match
`tna::centralities(measures = "Diffusion")`.

## See also

[`centrality_expected`](https://sonsoles.me/cograph/reference/centrality_expected.md),
[`centrality_diffusion_centrality`](https://sonsoles.me/cograph/reference/centrality_diffusion_centrality.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_diffusion(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>         36         42         49         40         34         37         31 
#>   Evaluate     Create      Share 
#>         39         44         40 
```
