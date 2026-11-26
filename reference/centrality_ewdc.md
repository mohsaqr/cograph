# Extended Weight Degree Centrality

Extended weight degree centrality (EWdc; Liu et al. 2016) applies the
construction of
[`centrality_wdc`](https://sonsoles.me/cograph/reference/centrality_wdc.md)
once more, with the weight degree centrality in the place of the degree:
\$\$EWdc(i) = \Bigl(\sum\_{j \in N(i)} Wdc(j) - Wdc(i)\Bigr)
Wdc(i)^{\alpha},\$\$ with the same \\\alpha\\, the absolute degree
assortativity by default.

## Usage

``` r
centrality_ewdc(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `wdc_alpha` (the exponent, default `NULL` for the
  absolute degree assortativity).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
`wdc_alpha` and the rule for an undefined assortativity are those of
[`centrality_wdc`](https://sonsoles.me/cograph/reference/centrality_wdc.md).
\\0^0\\ is taken as 1, so with \\\alpha = 0\\ a node whose weight degree
centrality is 0 scores the sum of its neighbors' values. The score can
be negative, for instance at the leaves of a star; the source does not
discuss this, and the value is kept. The source prints no value on a
network available here, so the implementation is checked against the
definition only. Isolated nodes score 0. A `wdc_alpha` that is negative
or not finite raises a `cograph_bad_parameter` error.
`normalized = TRUE` divides the scores by their maximum.

## References

Liu, Y., Wei, B., Du, Y., Xiao, F. and Deng, Y. (2016). Identifying
influential spreaders by weight degree centrality in complex networks.
Chaos, Solitons & Fractals, 86, 1-7.
[doi:10.1016/j.chaos.2016.01.030](https://doi.org/10.1016/j.chaos.2016.01.030)
.

## See also

[`centrality_wdc`](https://sonsoles.me/cograph/reference/centrality_wdc.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_ewdc(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   343.6508   447.8242   517.5218   425.3791   336.9968   347.8979   287.4802 
#>   Evaluate     Create      Share 
#>   384.7428   455.3340   388.2400 
```
