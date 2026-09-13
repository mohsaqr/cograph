# Global structure model centrality

The Global Structure Model (GSM) of Ullah et al. (2021) is
\\GSM(i)=\exp(k_s(i)/N)\sum\_{j\ne i}k_s(j)/d\_{ij}\\, where k_s denotes
original graph core numbers and d denotes hop distances. It combines a
focal coreness factor with distance-discounted coreness of other nodes.
N is the total original node count, including isolates.

## Usage

``` r
centrality_global_structure(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  `normalized = TRUE` divides final scores by their maximum.

## Value

Named numeric vector in input node order.

## Details

Both GSM and
[`centrality_hybrid_global_structure`](https://sonsoles.me/cograph/reference/centrality_hybrid_global_structure.md)
use the simple undirected skeleton, ignoring weights, mode, path
inversion and distance cutoffs. Loops are removed and parallel
connections count once. Only reachable partners contribute; this is an
explicit disconnected-graph extension. Isolates and singletons score
zero, empty input returns an empty vector. Other components can affect
results through the global node count and, for H-GSM, its global mean.
These are not independent per-component calculations.

Production uses native coreness and all-pairs distance kernels, with
worst-case O(N^3) time and O(N^2) memory. Numerical verification uses
independent NetworkX cores/distances and exhaustive small-graph oracles.
Agreement with a numerical definition does not establish author-software
parity or superior epidemic-spreading predictions.

## References

Ullah, A., Wang, B., Sheng, J., Long, J., Khan, N., & Sun, Z. (2021).
Identification of nodes influence based on global structure model in
complex networks. Scientific Reports, 11, 6173, equations 5-8.
[doi:10.1038/s41598-021-84684-x](https://doi.org/10.1038/s41598-021-84684-x)
.

## Examples

``` r
centrality_global_structure(igraph::make_ring(4))
#>        1        2        3        4 
#> 8.243606 8.243606 8.243606 8.243606 
```
