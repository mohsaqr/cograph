# Two-Step Expected Influence

Two-step expected influence (Robinaugh, Millner and McNally 2016) adds
to the one-step expected influence of a node the one-step expected
influence of its neighbors, each weighted by the signed edge between
them: \$\$EI_2(i) = EI_1(i) + \sum\_{j} w\_{ij} EI_1(j).\$\$

## Usage

``` r
centrality_expected_influence_2(x, mode = "out", ...)
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

`mode = "out"` (the default here) follows outgoing edges at both steps,
`mode = "in"` incoming edges and `mode = "all"` both, with a self-loop
counted once. On an undirected network the three modes agree and each
edge is counted once. `weighted = FALSE` gives every edge weight one.
When the network has a negative edge, `normalized = TRUE` divides by the
largest absolute score and keeps the sign.

## References

Robinaugh, D. J., Millner, A. J., & McNally, R. J. (2016). Identifying
highly influential nodes in the complicated grief network. Journal of
Abnormal Psychology, 125(6), 747-757.
[doi:10.1037/abn0000181](https://doi.org/10.1037/abn0000181) .

## See also

[`centrality_expected_influence_1`](https://sonsoles.me/cograph/reference/centrality_expected_influence_1.md),
[`centrality_strength`](https://sonsoles.me/cograph/reference/centrality_strength.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_expected_influence_2(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>     0.9843     2.9500     1.0005     1.3342     0.3105     1.1762     0.8949 
#>   Evaluate     Create      Share 
#>     1.3586     1.6813     1.9896 
```
