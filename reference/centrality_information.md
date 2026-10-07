# Information Centrality

Information centrality (Stephenson and Zelen 1989) measures the
information carried by all paths between a node and the others, each
path weighted by its length. With \\C = B^{-1}\\, where \\B\\ has
diagonal \\1 + s_i\\ (\\s_i\\ the strength) and off-diagonal entries
\\1 - w\_{ij}\\, \$\$I_i = \frac{1}{C\_{ii} + (T - 2R_i)/n},\$\$ where
\\T\\ is the trace of \\C\\ and \\R_i\\ the sum of row \\i\\.

## Usage

``` r
centrality_information(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (default `TRUE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The network is symmetrized with \\(w\_{ij} + w\_{ji})/2\\, so direction
is ignored. Edge weights enter as tie strengths, and `weighted = FALSE`
uses the binary matrix. Isolated nodes score 0 and are left out of
\\n\\. When \\B\\ is singular, as on some disconnected networks, every
score is `NA` without a warning. On unweighted undirected networks the
values equal
[`sna::infocent()`](https://rdrr.io/pkg/sna/man/infocent.html).

## References

Stephenson, K., & Zelen, M. (1989). Rethinking centrality: Methods and
examples. *Social Networks*, 11(1), 1-37.

## See also

[`centrality_current_flow_closeness`](https://sonsoles.me/cograph/reference/centrality_current_flow_closeness.md),
[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_information(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.3947689  0.4512245  0.4449921  0.4508582  0.3776635  0.4159092  0.2730538 
#>   Evaluate     Create      Share 
#>  0.4250643  0.4172676  0.4531011 
```
