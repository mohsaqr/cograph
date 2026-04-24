# Expected Influence (two-step)

Two-step signed-weight sum: a node's own expected influence (EI1) plus
the weighted sum of its neighbors' EI1 (Robinaugh, Millner & McNally
2016). Captures both the node's direct influence and the influence it
exerts indirectly via highly-connected neighbors.

## Usage

``` r
centrality_expected_influence_2(x, mode = "out", ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  One of "all", "in", "out" for directed graphs. Default "out".

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of two-step expected-influence values.

## References

Robinaugh DJ, Millner AJ, McNally RJ (2016). Identifying highly
influential nodes in the complicated grief network. *Journal of Abnormal
Psychology*, 125(6), 747-757.

## See also

[`centrality_expected_influence_1`](https://sonsoles.me/cograph/reference/centrality_expected_influence_1.md)
for the one-step variant.

## Examples

``` r
W <- matrix(c( 0.0,  0.5, -0.3,  0.2,
               0.5,  0.0,  0.4, -0.1,
              -0.3,  0.4,  0.0,  0.6,
               0.2, -0.1,  0.6,  0.0), 4, 4, byrow = TRUE)
rownames(W) <- colnames(W) <- c("A", "B", "C", "D")
centrality_expected_influence_2(W)
#>    A    B    C    D 
#> 0.73 1.21 1.32 1.12 
```
