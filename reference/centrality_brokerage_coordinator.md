# Gould-Fernandez Brokerage — Coordinator Role

Coordinator brokerage (w_I): count of open directed 2-paths \\A \to V
\to A\\ passing through node \\V\\, where all three nodes belong to
\\V\\'s group. The broker mediates contact between two in-group members.

## Usage

``` r
centrality_brokerage_coordinator(x, membership = NULL, ...)
```

## Arguments

- x:

  Directed network input (matrix, igraph, cograph_network, tna object).

- membership:

  Integer or character vector of group assignments, length equal to the
  number of nodes. Required.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named integer vector of coordinator role counts.

## Details

Bit-exact match against `sna::brokerage$raw.nli[, "w_I"]`. Counts OPEN
2-paths only — those where no direct edge from `a` to `c` exists.
Directed-only; returns `NA` with a warning on undirected input.

## References

Gould, R. V., & Fernandez, R. M. (1989). Structures of mediation: A
formal approach to brokerage in transaction networks. *Sociological
Methodology*, 19, 89-126.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
[`centrality_brokerage_itinerant`](https://sonsoles.me/cograph/reference/centrality_brokerage_itinerant.md),
[`centrality_brokerage_representative`](https://sonsoles.me/cograph/reference/centrality_brokerage_representative.md),
[`centrality_brokerage_gatekeeper`](https://sonsoles.me/cograph/reference/centrality_brokerage_gatekeeper.md),
[`centrality_brokerage_liaison`](https://sonsoles.me/cograph/reference/centrality_brokerage_liaison.md).

## Examples

``` r
adj <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
centrality_brokerage_coordinator(adj, membership = c(1, 1, 2, 2))
#> A B C D 
#> 0 0 0 0 
```
