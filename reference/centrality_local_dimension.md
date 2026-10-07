# Local Dimension

The local dimension (Silva and Costa 2013; Pu et al. 2014) is the growth
exponent of the ball around a node. With \\B_i(r)\\ the number of nodes
within \\r\\ hops of \\i\\, the node itself included, it is the
least-squares slope of \\\ln B_i(r)\\ on \\\ln r\\ over \\r = 1, \ldots,
d\_{\max}(i)\\: \$\$D_i = \frac{d \ln B_i(r)}{d \ln r}.\$\$

## Usage

``` r
centrality_local_dimension(x, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Distances are hop counts, so edge weights are ignored. On a directed
network `mode` sets the direction of the paths. A node that reaches most
of the network in a few hops has a small exponent, so lower values mark
more influential nodes. A node with a single radius returns the
discretized derivative \\r\\ n_i(r) / B_i(r)\\ at \\r = 1\\, where
\\n_i(r)\\ counts the nodes at distance exactly \\r\\. A node that
reaches no other node returns `NaN`.

## References

Silva, F. N., & Costa, L. da F. (2013). Local dimension of complex
networks. arXiv:1209.2476.

Pu, J., Chen, X., Wei, D., Liu, Q., & Deng, Y. (2014). Identifying
influential nodes based on local dimension. EPL, 107(1), 10010.

Wen, T., & Jiang, W. (2019). Identifying influential nodes based on
fuzzy local dimension in complex networks. Chaos, Solitons & Fractals,
119, 332-342.

## See also

[`centrality_local_information_dimension`](https://sonsoles.me/cograph/reference/centrality_local_information_dimension.md),
[`centrality_local_dimension_fixed`](https://sonsoles.me/cograph/reference/centrality_local_dimension_fixed.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_local_dimension(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.7369656  0.5145732  0.3219281  0.5145732  0.7369656  0.7369656  1.0000000 
#>   Evaluate     Create      Share 
#>  0.7369656  0.5145732  0.7369656 
```
