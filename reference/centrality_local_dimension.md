# Local Dimension

Growth exponent of the ball around a node (Silva & Costa 2013; Pu et al.
2014). Let \\B_i(r)\\ be the number of nodes within \\r\\ hops of \\i\\,
the node itself included. The local dimension is the slope of \\\ln
B_i(r)\\ on \\\ln r\\ over \\r = 1, \ldots, d\_{\max}(i)\\: \$\$D_i =
\frac{d \ln B_i(r)}{d \ln r}.\$\$ A node that reaches most of the
network in a few hops has a small exponent, so **lower values mark more
influential nodes**. When a node has a single radius (it reaches every
other node in one hop) the regression is undefined and the discretized
derivative \\r\\ n_i(r) / B_i(r)\\ at \\r = 1\\ is reported, where
\\n_i(r)\\ counts the nodes at distance exactly \\r\\.

## Usage

``` r
centrality_local_dimension(x, mode = "all", ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"out"` (distances along
  out-edges), or `"in"`.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector, one value per node. `NaN` for a node that reaches
no other node.

## Details

The implementation reproduces the worked example in Wen & Jiang (2019),
which reports 0.9231 for ring sizes 4, 5, 4, 4. Distances are hop
counts; edge weights are ignored.

## References

Silva, F. N., & Costa, L. da F. (2013). Local dimension of complex
networks. arXiv:1209.2476.

Pu, J., Chen, X., Wei, D., Liu, Q., & Deng, Y. (2014). Identifying
influential nodes based on local dimension. EPL, 107(1), 10010.

Wen, T., & Jiang, W. (2019). Identifying influential nodes based on
fuzzy local dimension in complex networks. Chaos, Solitons & Fractals,
119, 332-342.

## See also

[`centrality_local_information_dimension`](https://sonsoles.me/cograph/reference/centrality_local_information_dimension.md)
for the entropy-weighted variant,
[`centrality_distance_entropy`](https://sonsoles.me/cograph/reference/centrality_distance_entropy.md).

## Examples

``` r
star5 <- matrix(0, 5, 5)
star5[1, 2:5] <- 1; star5[2:5, 1] <- 1
rownames(star5) <- colnames(star5) <- LETTERS[1:5]
centrality_local_dimension(star5)
#>        A        B        C        D        E 
#> 0.800000 1.321928 1.321928 1.321928 1.321928 
```
