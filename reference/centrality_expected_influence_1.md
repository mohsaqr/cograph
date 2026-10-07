# One-Step Expected Influence

One-step expected influence (Robinaugh, Millner and McNally 2016) sums
the signed weights of a node's edges: \$\$EI_1(i) = \sum\_{j}
w\_{ij}.\$\$ Negative edges lower the score, which makes the measure
suited to partial-correlation and other signed networks.

## Usage

``` r
centrality_expected_influence_1(x, mode = "out", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"out"` (default), `"in"` or `"all"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized` and `psych_network`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

`mode = "out"` (the default here) sums the outgoing weights,
`mode = "in"` the incoming weights and `mode = "all"` both, with a
self-loop counted once. On an undirected network `"out"` and `"in"`
agree, and `"all"` counts every edge twice. Edge weights are always
used, and `weighted = FALSE` has no effect. On an unweighted input the
score is the degree in the chosen mode. When the network has a negative
edge, `normalized = TRUE` divides by the largest absolute score and
keeps the sign.

## References

Robinaugh, D. J., Millner, A. J., & McNally, R. J. (2016). Identifying
highly influential nodes in the complicated grief network. Journal of
Abnormal Psychology, 125(6), 747-757.
[doi:10.1037/abn0000181](https://doi.org/10.1037/abn0000181) .

## See also

[`centrality_expected_influence_2`](https://sonsoles.me/cograph/reference/centrality_expected_influence_2.md),
[`centrality_strength`](https://sonsoles.me/cograph/reference/centrality_strength.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_expected_influence_1(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>       0.62       1.58       0.53       0.79       0.20       0.79       0.60 
#>   Evaluate     Create      Share 
#>       0.83       0.93       1.09 
```
