# Extended hybrid characteristic centrality

The extended hybrid characteristic centrality of Liu and Zheng is the
closed-neighbourhood sum of
[`centrality_hcc`](https://sonsoles.me/cograph/reference/centrality_hcc.md):
\\EHCC(u)=HCC(u)+\sum\_{v\in\phi(u)}HCC(v)\\, the focal node counted
once and each neighbour of the open 1-order neighbourhood once. It
rewards a node whose neighbours are themselves high in both the extended
degree and the E-shell hierarchy, which a node can be without being high
itself.

## Usage

``` r
centrality_ehcc(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  including `hcc_delta`.

## Value

Named numeric vector in input node order.

## Details

Everything recorded on
[`centrality_hcc`](https://sonsoles.me/cograph/reference/centrality_hcc.md)
carries over unchanged: the source's \\\arg\max\\/\\\arg\min\\ typo in
step 3 of the E-shell procedure, the original-graph reading of
\\k^{ex}\\ and \\k^{ex}\_{max}\\ against the residual-graph peel, the
global and therefore not component-local normalisers, the `hcc_delta`
domain \\\[0,1\]\\, the \\0/0\\ of an edgeless graph written as zero,
the simple undirected unweighted skeleton, and the ignored weights,
mode, cutoff and inversion. Because HCC lies in \\\[0,2\]\\, EHCC lies
in \\\[0,2(1+k\_{max})\]\\, and an isolate scores exactly its own HCC.

## References

Liu, J. and Zheng, J. (2023). Identifying important nodes in complex
networks based on extended degree and E-shell hierarchy decomposition.
Scientific Reports, 13, 3197. Equation (5) on page 3.
[doi:10.1038/s41598-023-30308-5](https://doi.org/10.1038/s41598-023-30308-5)
.

## See also

[`centrality_hcc`](https://sonsoles.me/cograph/reference/centrality_hcc.md)
for the summand and
[`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)
for the catalogue.

## Examples

``` r
# On a regular graph every node scores 2, so EHCC is 2 (1 + k).
centrality_ehcc(igraph::make_ring(6))
#> 1 2 3 4 5 6 
#> 6 6 6 6 6 6 

# The star's centre collects every leaf's score as well as its own.
centrality_ehcc(igraph::make_star(6, mode = "undirected"))
#>   1   2   3   4   5   6 
#> 7.5 3.1 3.1 3.1 3.1 3.1 
```
