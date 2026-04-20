# Extract Triads with Node Labels

Extract all triads from a network, preserving node labels. This allows
users to see which specific node combinations form each motif pattern.

## Usage

``` r
extract_triads(
  x,
  type = NULL,
  involving = NULL,
  threshold = 0,
  min_total = 5,
  directed = NULL
)
```

## Arguments

- x:

  A matrix, igraph object, tna, or cograph_network

- type:

  Character vector of MAN codes to filter by (e.g., "030T", "030C").
  Default NULL returns all types.

- involving:

  Character vector of node labels. Only return triads involving at least
  one of these nodes. Default NULL returns all triads.

- threshold:

  Minimum edge weight for an edge to be considered present. Type is
  determined by edges with weight \> threshold. Default 0.

- min_total:

  Minimum total weight across all 6 edges. Excludes trivial triads with
  low overall activity. Default 5.

- directed:

  Logical. Treat network as directed? Default auto-detected.

## Value

A data frame with columns:

- A, B, C:

  Node labels for the three nodes in the triad

- type:

  MAN code (003, 012, ..., 300)

- weight_AB, weight_BA, weight_AC, weight_CA, weight_BC, weight_CB:

  Edge weights (frequencies) for all 6 possible directed edges

- total_weight:

  Sum of all 6 edge weights

## Details

This function complements
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md)
by showing the actual node combinations that form each motif pattern. A
typical workflow is:

1.  Use
    [`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md)
    to identify over/under-represented patterns

2.  Use `extract_triads()` with `type` filter to see which nodes form
    those patterns

3.  Sort by `total_weight` to find the strongest triads

**Type vs Weight distinction:**

- **Type** is determined by edge presence (weight \> threshold)

- **Weights** are the actual frequency counts, useful for ranking triads
  by strength

## See also

[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md),
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md)

Other motifs:
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md),
[`get_edge_list()`](https://sonsoles.me/cograph/reference/get_edge_list.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md),
[`plot.cograph_motif_analysis()`](https://sonsoles.me/cograph/reference/plot.cograph_motif_analysis.md),
[`plot.cograph_motifs()`](https://sonsoles.me/cograph/reference/plot.cograph_motifs.md),
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`triad_census()`](https://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
mat <- matrix(c(0,3,2,0, 0,0,5,1, 0,0,0,4, 2,0,0,0), 4, 4, byrow = TRUE)
rownames(mat) <- colnames(mat) <- c("Plan", "Execute", "Monitor", "Adapt")
net <- as_cograph(mat)

# All triads, feed-forward loops, triads involving "Plan"
head(extract_triads(net))
#>         A       B       C type weight_AB weight_BA weight_AC weight_CA
#> 1    Plan Execute Monitor 030T         3         0         2         0
#> 2    Plan Execute   Adapt 030C         3         0         0         2
#> 3    Plan Monitor   Adapt 030C         2         0         0         2
#> 4 Execute Monitor   Adapt 030T         5         0         1         0
#>   weight_BC weight_CB total_weight
#> 1         5         0           10
#> 2         1         0            6
#> 3         4         0            8
#> 4         4         0           10
extract_triads(net, type = "030T")
#>         A       B       C type weight_AB weight_BA weight_AC weight_CA
#> 1    Plan Execute Monitor 030T         3         0         2         0
#> 2 Execute Monitor   Adapt 030T         5         0         1         0
#>   weight_BC weight_CB total_weight
#> 1         5         0           10
#> 2         4         0           10
extract_triads(net, involving = "Plan")
#>      A       B       C type weight_AB weight_BA weight_AC weight_CA weight_BC
#> 1 Plan Execute Monitor 030T         3         0         2         0         5
#> 2 Plan Execute   Adapt 030C         3         0         0         2         1
#> 3 Plan Monitor   Adapt 030C         2         0         0         2         4
#>   weight_CB total_weight
#> 1         0           10
#> 2         0            6
#> 3         0            8
```
