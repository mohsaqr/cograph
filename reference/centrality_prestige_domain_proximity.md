# Domain Proximity Prestige

Distance-weighted variant of domain prestige. For each directed node
\\v\\: \$\$PD(v) = R_v^2 / (D_v \cdot (n - 1))\$\$ where \\R_v\\ is the
number of other nodes that reach \\v\\, and \\D_v\\ is the sum of
geodesic distances from those reachers to \\v\\. A node that is
reachable quickly from many others scores high; unreachable nodes score
0.

## Usage

``` r
centrality_prestige_domain_proximity(x, ...)
```

## Arguments

- x:

  Directed network input (matrix, igraph, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of domain proximity prestige values in \\\[0,
1\]\\.

## Details

Bit-exact match against `sna::prestige(cmode = "domain.proximity")` on
strongly connected directed graphs. Directed-only; returns `NA` with a
warning on undirected input.

## Divergence from sna on disconnected graphs

sna's formula computes `(counts > 0) * gdist` element-wise and then sums
to get the denominator. For any pair where `gdist = Inf` (unreachable),
R evaluates `FALSE * Inf = NaN`, so the entire denominator becomes `NaN`
and sna zeros every node via `p[is.nan(p)] <- 0`. cograph masks with
[`is.finite()`](https://rdrr.io/r/base/is.finite.html) before summing,
producing mathematically correct values on any directed graph, including
those with disconnected components.

## References

Wasserman, S., & Faust, K. (1994). *Social Network Analysis: Methods and
Applications*. Cambridge University Press.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
[`centrality_prestige_domain`](https://sonsoles.me/cograph/reference/centrality_prestige_domain.md)
for the unweighted count,
[`centrality_reaching_local`](https://sonsoles.me/cograph/reference/centrality_reaching_local.md)
for the dual out-reachability measure.

## Examples

``` r
# Directed 3-cycle: each node is reached by both others at distance 1 and 2
adj <- matrix(c(0,1,0, 0,0,1, 1,0,0), 3, 3, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_prestige_domain_proximity(adj)
#>         A         B         C 
#> 0.6666667 0.6666667 0.6666667 
```
