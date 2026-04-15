# Information Centrality (Stephenson-Zelen)

Information centrality (Stephenson & Zelen 1989) measures a node's
importance in terms of the "information" contained in all paths (not
only shortest) passing through it. Defined via the inverse of a
Laplacian-like matrix, yielding per-node \\IC_i = 1 / (C\_{ii} +
(\mathrm{tr}(C) - 2 R_i) / n)\\ where \\C = A^{-1}\\ and \\R_i\\ is the
row sum of \\C\\.

## Usage

``` r
centrality_information(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of information centrality values.

## Details

Bit-exact match against
[`sna::infocent`](https://rdrr.io/pkg/sna/man/infocent.html) on
connected undirected graphs (cograph mirrors sna's exact construction
and call sequence).

## References

Stephenson, K., & Zelen, M. (1989). Rethinking centrality: Methods and
examples. *Social Networks*, 11(1), 1-37.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
[`centrality_current_flow_closeness`](https://sonsoles.me/cograph/reference/centrality_current_flow_closeness.md).

## Examples

``` r
adj <- matrix(c(0,1,1,0, 1,0,1,1, 1,1,0,1, 0,1,1,0), 4, 4)
rownames(adj) <- colnames(adj) <- LETTERS[1:4]
centrality_information(adj)
#>        A        B        C        D 
#> 1.777778 2.285714 2.285714 1.777778 
```
