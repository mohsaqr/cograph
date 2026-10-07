# Two-Way Random Walk Betweenness

Two-way random walk betweenness (Curado et al. 2022) counts how often a
node lies on the strongest two-way route between a pair. For every
unordered pair \\(i, j)\\ the two-step transfer \\P\_{itj} = w\_{it}
w\_{tj} / (d_i d_j)\\ is combined into \\T\_{ij}\[t, k\] = P\_{itj}
P\_{jki}\\, the diagonal is dropped, and the largest entry credits one
count to \\t\\ and one to \\k\\. The score of a node is its total count
over all pairs.

## Usage

``` r
centrality_two_way_rw(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (use edge weights, default `TRUE`).

## Value

A named numeric vector with one count per node, in input node order.

## Details

Edge weights are used, and direction and self-loops are ignored. Ties in
the maximum go to the first entry in row-major order. Nodes that never
lie on a winning two-way route score 0. The source divides the transfer
by \\d_i d_j\\, where a random-walk probability would divide by \\d_i
d_t\\. The formula is implemented as printed.

## References

Curado, M., Rodriguez, R., Tortosa, L., & Vicent, J. F. (2022). A new
centrality measure in dense networks based on two-way random walk
betweenness. Applied Mathematics and Computation, 412, 126560.

## See also

[`centrality_current_flow_betweenness`](https://sonsoles.me/cograph/reference/centrality_current_flow_betweenness.md),
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_two_way_rw(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          5          9          8         14          6          5          2 
#>   Evaluate     Create      Share 
#>          7          6         10 
```
