# Two-Way Random Walk Betweenness

Curado, Rodriguez, Tortosa and Vicent's (2022) counting measure. For
every unordered pair \\(i, j)\\ the two-step transfer \\P\_{itj} =
w\_{it} w\_{tj} / (d_i d_j)\\ (zero when any two of the three coincide)
is combined into \\T\_{ij}\[t, k\] = P\_{itj} P\_{jki}\\, the diagonal
is dropped, and the single largest entry credits one count to \\t\\ and
one to \\k\\. A node's score is its total count over all pairs. Higher =
more central; nodes never on a winning two-way route score 0, so sparse
tails are not ranked. Reproduces the paper's toy example exactly,
including every printed fraction.

## Usage

``` r
centrality_two_way_rw(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of counts, one per node.

## Details

The paper's \\P\_{itj}\\ is not a random-walk probability (its
denominator is \\d_i d_j\\, not \\d_i d_t\\); it is implemented as
printed. Ties in the maximum go to the first entry in row-major order.
Edge weights are used; direction and loops are ignored. Cost is
\\O(n^4)\\: fine to a few hundred nodes, slow beyond.

## References

Curado, M., Rodriguez, R., Tortosa, L., & Vicent, J. F. (2022). A new
centrality measure in dense networks based on two-way random walk
betweenness. Applied Mathematics and Computation, 412, 126560.

## See also

[`centrality_current_flow_betweenness`](https://sonsoles.me/cograph/reference/centrality_current_flow_betweenness.md)
for Newman's random-walk betweenness.

## Examples

``` r
adj <- matrix(0, 6, 6)
adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
adj <- adj + t(adj)
rownames(adj) <- colnames(adj) <- LETTERS[1:6]
centrality_two_way_rw(adj)
#> A B C D E F 
#> 0 0 0 0 0 0 
```
