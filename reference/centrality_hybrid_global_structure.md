# Hybrid global structure model centrality

Mukhtar et al.'s H-GSM (2023) uses \\s_i=\exp(k_s(i)k_i/N)\\,
\\a=\lceil\log_2(N^{-1}\sum_i s_i)\rceil\\, and
\\H\text{-}GSM(i)=s_i\sum\_{j\ne i}s_j/d\_{ij}^{a}\\. k_i is simple
degree, k_s(i) is original coreness, and d is hop distance. The ceiling
exponent is computed from the mean self-influence over ALL original
nodes, including isolates whose self-influence is one. The factor s_i
alone is not the final centrality score.

## Usage

``` r
centrality_hybrid_global_structure(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in input node order.

## Details

Topology and disconnected-graph conventions are shared with
[`centrality_global_structure`](https://sonsoles.me/cograph/reference/centrality_global_structure.md).
The adaptive exponent is used exactly as specified, including its
discontinuities at powers of two; it is not smoothed or replaced by a
fixed exponent.

Self-influence, its mean and final sums are evaluated in logarithmic
form. Raw scores exceeding double precision raise an error. With
`normalized = TRUE`, final scores are computed directly as exponentials
of log-score differences, so normalized results remain available even
when raw scores overflow. Extremely small normalized ratios may
underflow to zero. Normalization is applied to the complete score, not
separately to self-influence or neighbor contributions.

## References

Mukhtar, M. F., et al. (2023). Integrating local and global information
to identify influential nodes in complex networks. Scientific Reports,
13, 11411, equations 6-8.
[doi:10.1038/s41598-023-37570-7](https://doi.org/10.1038/s41598-023-37570-7)
.

## Examples

``` r
centrality_hybrid_global_structure(igraph::make_ring(4))
#>        1        2        3        4 
#> 16.62538 16.62538 16.62538 16.62538 
```
