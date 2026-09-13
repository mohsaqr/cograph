# X-degree centrality

Computes Torres et al.'s X-degree (equation 3.15): \$\$Xdeg(i) =
(\sum\_{j\in N(i)}(d_j-1))^2 - \sum\_{j\in N(i)}(d_j-1)^2.\$\$ Degrees
are measured in the original simple undirected graph. The score counts
oriented nonbacktracking walks of four edges whose middle vertex is i.
Walks can revisit a vertex provided they do not immediately reverse an
edge. It is also the sum of entries of the paper's matrix DFE, where D,
F and E are blocks of the nonbacktracking matrix around i.

## Usage

``` r
centrality_x_degree(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  `normalized = TRUE` divides scores by their maximum; an all-zero
  result stays zero.

## Value

Named numeric vector in input node order.

## Details

Uses the simple undirected skeleton: direction, weights, mode, inversion
and cutoff do not affect results. Loops are removed and parallel edges
count once. This projection is a cograph convention extending the
published simple, unweighted, undirected domain. Isolates and leaves
score zero; every vertex of a star also scores zero. Empty graphs return
no scores. Disconnected components are independent before maximum
normalization. These cases follow directly from the local formula.

Native arithmetic accumulates nonnegative pair products instead of
subtracting two squares. Aggregation takes O(n+m) time after neighbor
construction; the current dense skeleton conversion uses O(n squared)
time and memory. This is a score on the supplied graph, not the paper's
iterative node-removal immunization algorithm. Agreement with the author
function and matrix definition does not establish immunization efficacy,
exact eigendrop prediction or an unconditional spectral upper bound.

## References

Torres, L., Chan, K. S., Tong, H., & Eliassi-Rad, T. (2021).
Nonbacktracking Eigenvalues under Node Removal: X-Centrality and
Targeted Immunization. SIAM Journal on Mathematics of Data Science,
3(2), 656-675. Proposition 3.8, equation 3.15.
[doi:10.1137/20M1352132](https://doi.org/10.1137/20M1352132) .

## Examples

``` r
centrality_x_degree(igraph::make_graph("Zachary"))
#>    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16 
#> 2540 1478 2646 1204  162  202  202  974 2224  288  162    0  150 2158  352  352 
#>   17   18   19   20   21   22   23   24   25   26   27   28   29   30   31   32 
#>   18  240  352  976  352  240  352  826   62   76   96  604  538  630 1064 1690 
#>   33   34 
#> 1984 2068 
```
