# Trophic Incoherence Parameter

Computes the trophic incoherence parameter \\q\\, which measures how
vertically ordered a directed network is (Johnson et al. 2014). For each
edge \\(u, v)\\, the trophic difference is \\x\_{uv} = s_v - s_u\\ where
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

Numeric scalar: the trophic incoherence parameter. `NA` when the network
has no edges or when some node cannot be reached from a basal node. An
undirected network returns `NA` with a warning.

## Details

Values near 0 indicate a coherent network, in which every edge rises by
about one level. High values indicate many level-skipping or downward
edges. Johnson et al. (2014) reported that food webs with low \\q\\ are
more stable.

Trophic levels are computed on the binary adjacency matrix, so edge
weights are ignored. The levels are defined only for a directed network
in which every node can be reached from a basal node (a node with no
incoming edges).

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
strong <- filter_edges(regulation_net, weight > 0.3, keep_isolates = FALSE)
trophic_incoherence(strong)
#> [1] 1.26085
```
