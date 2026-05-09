# Expected Influence (one-step)

Signed-weight sum of a node's edges (Robinaugh, Millner & McNally 2016).
The appropriate centrality for networks with positive *and* negative
edges (partial-correlation, glasso, signed correlation networks) where
treating negative edges as positive magnitudes can be misleading.

## Usage

``` r
centrality_expected_influence_1(x, mode = "out", ...)
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

Named numeric vector of expected-influence values (signed).

## References

Robinaugh DJ, Millner AJ, McNally RJ (2016). Identifying highly
influential nodes in the complicated grief network. *Journal of Abnormal
Psychology*, 125(6), 747-757.

## See also

[`centrality_expected_influence_2`](https://sonsoles.me/cograph/reference/centrality_expected_influence_2.md)
for the two-step variant,
[`centrality_strength`](https://sonsoles.me/cograph/reference/centrality_strength.md)
for the weighted-degree analogue.

## Examples

``` r
# Signed weight matrix (partial correlations, for example)
W <- matrix(c( 0.0,  0.5, -0.3,  0.2,
               0.5,  0.0,  0.4, -0.1,
              -0.3,  0.4,  0.0,  0.6,
               0.2, -0.1,  0.6,  0.0), 4, 4, byrow = TRUE)
rownames(W) <- colnames(W) <- c("A", "B", "C", "D")
centrality_expected_influence_1(W)
#>   A   B   C   D 
#> 0.4 0.8 0.7 0.7 
```
