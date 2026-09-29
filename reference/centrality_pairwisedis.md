# Pairwise Disconnectivity (Potapov et al. 2008)

For a directed network, `pairwisedis(v)` is the fraction of ordered
reachable pairs \\(s, t)\\ that become unreachable when node \\v\\ is
removed: \$\$PD(v) = (\|P(G)\| - \|P(G - v)\|) / \|P(G)\|\$\$ where
\\\|P(G)\|\\ is the number of ordered pairs \\(s, t), s \ne t\\ with a
directed path from \\s\\ to \\t\\.

## Usage

``` r
centrality_pairwisedis(x, ...)
```

## Arguments

- x:

  Directed network input (matrix, igraph, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of pairwise disconnectivity values in \\\[0, 1\]\\.

## Details

Bit-exact match against
[`centiserve::pairwisedis`](https://rdrr.io/pkg/centiserve/man/pairwisedis.html)
on directed graphs. Requires the input to be directed; returns `NA` with
a warning on undirected inputs.

## References

Potapov, A. P., Goemann, B., & Wingender, E. (2008). The pairwise
disconnectivity index as a new metric for the topological analysis of
regulatory networks. *BMC Bioinformatics*, 9, 227.
[doi:10.1186/1471-2105-9-227](https://doi.org/10.1186/1471-2105-9-227) .

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
[`robustness`](https://sonsoles.me/cograph/reference/robustness.md).

## Examples

``` r
adj <- matrix(c(0,1,0, 0,0,1, 1,0,0), 3, 3, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_pairwisedis(adj)
#>         A         B         C 
#> 0.8333333 0.8333333 0.8333333 
```
