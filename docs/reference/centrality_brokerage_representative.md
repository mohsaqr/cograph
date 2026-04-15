# Gould-Fernandez Brokerage — Representative Role

Representative brokerage (b_IO): count of open directed 2-paths \\A \to
V \to B\\ where \\A\\ and \\V\\ are in the same group and \\B\\ is in a
different group. The broker represents their group outward.

## Usage

``` r
centrality_brokerage_representative(x, membership = NULL, ...)
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

Named integer vector of representative role counts.

## Details

Bit-exact match against `sna::brokerage$raw.nli[, "b_IO"]`.
Directed-only.

## References

Gould & Fernandez (1989).

## See also

[`centrality_brokerage_coordinator`](https://sonsoles.me/cograph/reference/centrality_brokerage_coordinator.md).

## Examples

``` r
adj <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
centrality_brokerage_representative(adj, membership = c(1, 1, 2, 2))
#> A B C D 
#> 0 1 0 1 
```
