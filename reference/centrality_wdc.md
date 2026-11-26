# Weight Degree Centrality

Weight degree centrality (Wdc; Liu et al. 2016) multiplies the number of
non-backtracking two-step walks from a node by a power of its degree:
\$\$Wdc(i) = \Bigl(\sum\_{j \in N(i)} k_j - k_i\Bigr) k_i^{\alpha},
\qquad \alpha = \|r\|,\$\$ with \\k\\ the degree and \\r\\ the degree
assortativity of the network.

## Usage

``` r
centrality_wdc(x, ...)
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
`wdc_alpha = NULL`, the default, takes \\\alpha = \|r\|\\ as the source
does; the source adds that other values can be used, and any single
finite nonnegative number is accepted. The assortativity is 0/0 when
every node with a tie has the same degree, on a regular network for
instance; \\\alpha\\ is then 0, the source's neutral case. With \\\alpha
= 0\\ the score is
[`centrality_con`](https://sonsoles.me/cograph/reference/centrality_con.md)
on a simple undirected network, the number of common neighbors a node
has with all the others together, and at the default it is that number
times the degree to the power \\\|r\|\\. The score ranks nodes much like
[`centrality_con`](https://sonsoles.me/cograph/reference/centrality_con.md),
[`centrality_expected`](https://sonsoles.me/cograph/reference/centrality_expected.md)
and
[`centrality_x_degree`](https://sonsoles.me/cograph/reference/centrality_x_degree.md)
(Kendall correlation up to 0.994 on four test networks, 0.86 at the
lowest). The source prints no value on a network available here, so the
implementation is checked against the definition only. Isolated nodes
score 0. A `wdc_alpha` that is negative or not finite raises a
`cograph_bad_parameter` error. `normalized = TRUE` divides the scores by
their maximum.

## References

Liu, Y., Wei, B., Du, Y., Xiao, F. and Deng, Y. (2016). Identifying
influential spreaders by weight degree centrality in complex networks.
Chaos, Solitons & Fractals, 86, 1-7.
[doi:10.1016/j.chaos.2016.01.030](https://doi.org/10.1016/j.chaos.2016.01.030)
.

## See also

[`centrality_ewdc`](https://sonsoles.me/cograph/reference/centrality_ewdc.md),
[`centrality_con`](https://sonsoles.me/cograph/reference/centrality_con.md),
[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_wdc(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   32.37679   39.97557   47.86470   38.43804   30.90512   33.84847   27.89810 
#>   Evaluate     Create      Share 
#>   36.79181   41.51309   36.79181 
centrality_wdc(regulation_net, wdc_alpha = 0)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>         22         26         30         25         21         23         20 
#>   Evaluate     Create      Share 
#>         25         27         25 
```
