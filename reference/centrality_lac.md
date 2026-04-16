# Local Average Connectivity (LAC)

Average degree of neighbors within the neighborhood subgraph. Measures
how interconnected a node's neighbors are. Proposed by Li et al. (2011)
for identifying essential proteins in PPI networks.

## Usage

``` r
centrality_lac(x, mode = "all", ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"in"`, or `"out"`.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `normalized`, `weighted`, `directed`).

## Value

Named numeric vector of LAC values.

## References

Li, M., Wang, J., Chen, X., Wang, H., & Pan, Y. (2011). A local average
connectivity-based method for identifying essential proteins from the
network level. *Computational Biology and Chemistry*, 35(3), 143-150.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_dmnc`](https://sonsoles.me/cograph/reference/centrality_dmnc.md)
for another neighborhood density measure.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_lac(adj)
#> A B C 
#> 1 1 1 
```
