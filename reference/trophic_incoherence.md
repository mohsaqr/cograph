# Trophic Incoherence Parameter

The trophic incoherence parameter \\q\\ is a measure of how "vertically
ordered" a directed network is (Johnson et al. 2014). For each edge
\\(u, v)\\, the trophic difference is \\x\_{uv} = s_v - s_u\\ where
\\s_i\\ is the trophic level of node \\i\\. The trophic incoherence
parameter is the (population) standard deviation of these differences:
\$\$q = \sqrt{\frac{1}{\|E\|} \sum\_{(u,v) \in E} (x\_{uv} -
\bar{x})^2}\$\$

## Usage

``` r
trophic_incoherence(x, cannibalism = TRUE)
```

## Arguments

- x:

  Directed network input.

- cannibalism:

  Logical. If `FALSE`, self-loops are removed before computing trophic
  differences. Default `TRUE`.

## Value

A single numeric value (`NA_real_` for empty edge sets or undirected
input).

## Details

Low values (\\q \approx 0\\) indicate a perfectly coherent network
(e.g., a pure food web where every edge goes up one level). High values
indicate an incoherent network with many level-skipping or downward
edges. Johnson et al. 2014 showed that low-\\q\\ food webs are
dynamically more stable.

Matches `networkx.trophic_incoherence_parameter` at machine epsilon.
Directed-only; requires at least one basal node (node with no incoming
edges) for trophic levels to be well-defined.

## References

Johnson, S., Dominguez-Garcia, V., Donetti, L., & Munoz, M. A. (2014).
Trophic coherence determines food-web stability. *PNAS*, 111(50),
17923-17928.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) (the
`trophic_level` measure) for the per-node levels used in the incoherence
calculation.

## Examples

``` r
# Small directed 3-node chain: 1 -> 2 -> 3 (perfectly coherent, q = 0)
adj <- matrix(c(0,1,0, 0,0,1, 0,0,0), 3, 3, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
trophic_incoherence(adj)
#> [1] 0
```
