# Dispersion (Backstrom-Kleinberg 2014)

Per-pair measure of tie strength from the Facebook
relationship-inference paper. For each pair \\(u, v)\\ where \\v\\ is a
neighbor of \\u\\:

## Usage

``` r
dispersion(x, u = NULL, v = NULL, normalized = TRUE, alpha = 1, b = 0, c = 0)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- u:

  Optional source node (1-based index or node name). If `NULL`
  (default), compute for all sources.

- v:

  Optional target node. If `NULL`, compute for all neighbors of `u`.

- normalized:

  Logical. If `TRUE` (default), return the normalized form; otherwise
  the raw count.

- alpha:

  Numeric normalization exponent. Default 1.

- b:

  Numeric bias added to dispersion before exponentiation. Default 0.

- c:

  Numeric bias added to embeddedness in the denominator. Default 0.

## Value

- Scalar if both `u` and `v` are specified.

- Named numeric vector if exactly one of `u`, `v` is given (names are
  the other endpoints).

- A data frame with columns `from`, `to`, `dispersion` when neither `u`
  nor `v` is given (one row per ordered edge).

## Details

1.  Let \\S_T = N(u) \cap N(v)\\ be their mutual friends (embeddedness).

2.  Count pairs \\(s, t) \subset S_T\\ such that:

    - \\s\\ and \\t\\ are not directly connected, AND

    - \\s\\ and \\t\\ share no common neighbor inside \\N(u)\\ other
      than \\u\\ and \\v\\.

3.  The raw dispersion is this count. When `normalized = TRUE`, the
    result is \\(\mathrm{dispersion} + b)^{\alpha} /
    (\mathrm{embeddedness} + c)\\ (normalization is skipped when
    `embeddedness + c == 0`).

Matches `networkx.dispersion` bit-exact for all three call modes (single
pair, single source, full matrix).

## References

Backstrom, L., & Kleinberg, J. (2014). Romantic partnerships and the
dispersion of social ties: A network analysis of relationship status on
Facebook. In *Proceedings of CSCW* (pp. 831-841). ACM.
<https://arxiv.org/pdf/1310.6753v1.pdf>

## Examples

``` r
g <- igraph::make_graph("Zachary")
# Node 0 (R index 1) to node 33 (R index 34)
dispersion(g, u = 1, v = 34)
#> [1] 1
# All pairs from node 1
head(dispersion(g, u = 1))
#>        2        3        4        5        6        7 
#> 2.142857 0.800000 0.800000 0.000000 0.000000 0.000000 
```
