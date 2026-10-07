# DegreeDiscountIC and SingleDiscount

DegreeDiscountIC and SingleDiscount (Chen, Wang and Yang 2009) select
spreaders for the independent-cascade model one at a time by the largest
discounted degree. After each selection every unselected neighbor \\v\\
of the new seed gains one selected neighbor \\t_v\\, and its
DegreeDiscountIC degree becomes \$\$dd_v = d_v - 2 t_v - (d_v - t_v)\\
t_v\\ p,\$\$ with propagation probability \\p\\. SingleDiscount uses
\\d_v - t_v\\.

## Usage

``` r
centrality_degree_discount(x, discount_p = 0.01, ...)

centrality_single_discount(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- discount_p:

  Propagation probability \\p\\ for DegreeDiscountIC (default 0.01).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Every node is placed, and the selection order is returned as a score.
The first node selected scores 1 and the last \\1/n\\, so scores lie in
\\(0, 1\]\\. Ties are broken by node order, which the source leaves
open. Direction, edge weights and self-loops are ignored.

## References

Chen, W., Wang, Y., & Yang, S. (2009). Efficient influence maximization
in social networks. Proceedings of the 15th ACM SIGKDD International
Conference on Knowledge Discovery and Data Mining, 199-208.

## See also

[`centrality_voterank`](https://sonsoles.me/cograph/reference/centrality_voterank.md),
[`centrality_ncvoterank`](https://sonsoles.me/cograph/reference/centrality_ncvoterank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_degree_discount(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        0.9        0.8        1.0        0.7        0.6        0.3        0.4 
#>   Evaluate     Create      Share 
#>        0.2        0.5        0.1 
centrality_single_discount(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        0.9        0.8        1.0        0.7        0.6        0.4        0.3 
#>   Evaluate     Create      Share 
#>        0.2        0.5        0.1 
```
