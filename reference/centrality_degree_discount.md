# DegreeDiscountIC and SingleDiscount Rankings

Chen, Wang and Yang's (2009) degree-discount heuristics for choosing
spreaders under the independent-cascade model. Nodes are selected one at
a time by the largest *discounted* degree; after each selection every
unselected neighbor \\v\\ of the new seed counts one more selected
neighbor, \\t_v\\, and its discounted degree becomes \$\$dd_v = d_v - 2
t_v - (d_v - t_v)\\ t_v\\ p\$\$ for DegreeDiscountIC (Algorithm 4 of the
paper, with propagation probability \\p\\, default 0.01), or simply
\\d_v - t_v\\ for SingleDiscount, where each neighbor of a new seed
discounts its degree by one. Every node is placed, so the result is a
full ranking, returned as a score: the first node selected scores 1, the
last \\1 / n\\.

## Usage

``` r
centrality_degree_discount(x, discount_p = 0.01, ...)

centrality_single_discount(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- discount_p:

  Propagation probability \\p\\ for DegreeDiscountIC. Default 0.01.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in \\(0, 1\]\\, one score per node.

## Details

Ties are broken by node order, which the paper does not specify.
Direction, edge weights and self-loops are ignored, as in the paper's
setting. Validated against an independent implementation of the
algorithm and against the reference code of the influence-maximization
literature on the karate club graph.

## References

Chen, W., Wang, Y., & Yang, S. (2009). Efficient influence maximization
in social networks. Proceedings of the 15th ACM SIGKDD International
Conference on Knowledge Discovery and Data Mining, 199-208.

## See also

[`centrality_voterank`](https://sonsoles.me/cograph/reference/centrality_voterank.md)
for the voting-based alternative.

## Examples

``` r
adj <- matrix(0, 6, 6)
adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
adj <- adj + t(adj)
rownames(adj) <- colnames(adj) <- LETTERS[1:6]
centrality_degree_discount(adj)
#>         A         B         C         D         E         F 
#> 0.6666667 0.3333333 1.0000000 0.1666667 0.8333333 0.5000000 
centrality_single_discount(adj)
#>         A         B         C         D         E         F 
#> 0.6666667 0.3333333 1.0000000 0.8333333 0.5000000 0.1666667 
```
