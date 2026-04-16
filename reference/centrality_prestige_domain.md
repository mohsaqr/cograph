# Domain Prestige

Directed-graph prestige measure: for each node \\v\\, the number of
other nodes that can reach \\v\\ via a directed path.
\$\$\mathrm{domain}(v) = \|\\u \ne v : u \to^\* v\\\|\$\$

## Usage

``` r
centrality_prestige_domain(x, ...)
```

## Arguments

- x:

  Directed network input (matrix, igraph, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of domain prestige values in \\\\0, 1, \ldots, N -
1\\\\.

## Details

Bit-exact match against `sna::prestige(cmode = "domain")`.
Directed-only; returns `NA` with a warning on undirected input.

## References

Wasserman, S., & Faust, K. (1994). *Social Network Analysis: Methods and
Applications*. Cambridge University Press.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
[`centrality_reaching_local`](https://sonsoles.me/cograph/reference/centrality_reaching_local.md)
for the dual "out-reachability" measure,
[`centrality_pairwisedis`](https://sonsoles.me/cograph/reference/centrality_pairwisedis.md)
for a related reachability-based directed measure.

## Examples

``` r
# Directed 3-cycle: every node reaches every other node
adj <- matrix(c(0,1,0, 0,0,1, 1,0,0), 3, 3, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_prestige_domain(adj)
#> A B C 
#> 2 2 2 
```
