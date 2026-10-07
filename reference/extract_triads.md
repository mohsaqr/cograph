# Extract Triads with Node Labels

Lists the triads of a network that contain at least one edge, with the
labels of their nodes, so that the node combinations forming each motif
pattern can be identified.

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

  A matrix, igraph object, tna, or cograph_network.

- type:

  Character vector of MAN codes to filter by (e.g., "030T", "030C").
  Default NULL returns all types.

- involving:

  Character vector of node labels. Only return triads involving at least
  one of these nodes. Default NULL returns all triads.

- threshold:

  Edge weight threshold. An edge counts as present for the triad type
  when its weight is greater than `threshold`. Default 0.

- min_total:

  Minimum total weight across all 6 edges. Excludes trivial triads with
  low overall activity. Default 5.

- directed:

  Logical or NULL. Whether the network is treated as directed. NULL
  (default) detects it from the input.

## Value

A data frame with one row per triad, in node index order, and the
columns:

- A, B, C:

  Labels of the three nodes in the triad.

- type:

  MAN code (012, ..., 300). Triads of type 003 have no edge and are not
  returned.

- weight_AB, weight_BA, weight_AC, weight_CA, weight_BC, weight_CB:

  Edge weights of the 6 possible directed edges.

- total_weight:

  Sum of the 6 edge weights.

A network with fewer than 3 nodes gives a data frame with no rows.

## Details

The function complements
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md)
by showing the node combinations that form each motif pattern. The triad
type is determined by edge presence (weight greater than `threshold`).
The weight columns hold the edge weights themselves, which measure the
strength of each triad.

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
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`triad_census()`](https://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
extract_triads(regulation_net, type = "030T", min_total = 0)
#>          A        B          C type weight_AB weight_BA weight_AC weight_CA
#> 1  Explore    Adapt    Discuss 030T      0.00      0.28      0.00      0.30
#> 2  Explore  Discuss     Create 030T      0.00      0.30      0.00      0.14
#> 3  Explore   Create      Share 030T      0.00      0.14      0.27      0.00
#> 4     Plan  Monitor Synthesize 030T      0.13      0.00      0.00      0.11
#> 5     Plan  Monitor   Evaluate 030T      0.13      0.00      0.49      0.00
#> 6     Plan  Discuss     Create 030T      0.40      0.00      0.20      0.00
#> 7     Plan Evaluate     Create 030T      0.49      0.00      0.20      0.00
#> 8  Monitor    Adapt   Evaluate 030T      0.16      0.00      0.00      0.33
#> 9  Monitor    Adapt      Share 030T      0.16      0.00      0.00      0.49
#> 10 Monitor  Reflect Synthesize 030T      0.00      0.15      0.00      0.07
#> 11 Monitor  Reflect   Evaluate 030T      0.00      0.15      0.00      0.33
#>    weight_BC weight_CB total_weight
#> 1       0.34      0.00         0.92
#> 2       0.14      0.00         0.58
#> 3       0.23      0.00         0.64
#> 4       0.00      0.07         0.31
#> 5       0.00      0.33         0.95
#> 6       0.14      0.00         0.74
#> 7       0.00      0.39         1.08
#> 8       0.00      0.43         0.92
#> 9       0.00      0.39         1.04
#> 10      0.00      0.42         0.64
#> 11      0.00      0.07         0.55
```
