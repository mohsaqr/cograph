# Network Motif Analysis

Analyze recurring subgraph patterns (motifs) in networks and test their
statistical significance against null models.

## Usage

``` r
motif_census(
  x,
  size = 3,
  n_random = 100,
  method = c("configuration", "gnm"),
  directed = NULL,
  seed = NULL
)

# S3 method for class 'cograph_motifs'
print(x, ...)
```

## Arguments

- x:

  A matrix, igraph object, or cograph_network

- size:

  Motif size: 3 (triads) or 4 (tetrads). Default 3.

- n_random:

  Number of random networks for null model. Default 100.

- method:

  Null model method: "configuration" (preserves degree) or "gnm"
  (preserves edge count). Default "configuration".

- directed:

  Logical. Treat as directed? Default auto-detected.

- seed:

  Random seed for reproducibility

## Value

A `cograph_motifs` object containing:

- `counts`: Motif counts in observed network

- `null_mean`: Mean counts in random networks

- `null_sd`: Standard deviation in random networks

- `z_scores`: Z-scores (observed - mean) / sd

- `p_values`: Two-tailed p-values

- `significant`: Logical vector (\|z\| \> 2)

- `size`: Motif size (3 or 4)

- `directed`: Whether network is directed

- `n_random`: Number of random networks used

## See also

[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md) for the
unified API,
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md)
for detailed triad extraction,
[`plot.cograph_motifs()`](https://sonsoles.me/cograph/reference/plot.cograph_motifs.md)
for plotting

Other motifs:
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](https://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](https://sonsoles.me/cograph/reference/get_edge_list.md),
[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md),
[`plot.cograph_motif_analysis()`](https://sonsoles.me/cograph/reference/plot.cograph_motif_analysis.md),
[`plot.cograph_motifs()`](https://sonsoles.me/cograph/reference/plot.cograph_motifs.md),
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`triad_census()`](https://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
# Create a directed network
mat <- matrix(c(
  0, 1, 1, 0,
  0, 0, 1, 1,
  0, 0, 0, 1,
  1, 0, 0, 0
), 4, 4, byrow = TRUE)

# Analyze triadic motifs
m <- motif_census(mat)
print(m)
#> Network Motif Analysis
#> Size: 3-node motifs (directed) | Null: configuration (n=100)
#> 
#>  motif count null_mean   null_sd    z_score    p_value significant
#>    003     0      0.00 0.0000000  0.0000000 1.00000000       FALSE
#>    012     0      0.00 0.0000000  0.0000000 1.00000000       FALSE
#>    102     0      0.13 0.3379977 -0.3846180 0.70052043       FALSE
#>   021D     0      0.00 0.0000000  0.0000000 1.00000000       FALSE
#>   021U     0      0.77 0.8973024 -0.8581277 0.39082196       FALSE
#>   021C     0      0.45 0.7299509 -0.6164798 0.53757787       FALSE
#>   111D     0      0.15 0.3588703 -0.4179783 0.67596296       FALSE
#>   111U     2      0.32 0.7369059  2.2798026 0.02261940        TRUE
#>   030T     0      0.06 0.2386833 -0.2513792 0.80152097       FALSE
#>   030C     0      0.51 0.7719188 -0.6606913 0.50881032       FALSE
#>    201     0      0.00 0.0000000  0.0000000 1.00000000       FALSE
#>   120D     2      0.43 0.7555184  2.0780433 0.03770537        TRUE
#>   120U     0      0.12 0.3265986 -0.3674235 0.71330317       FALSE
#>   120C     0      0.03 0.1714466 -0.1749816 0.86109410       FALSE
#>    210     0      0.00 0.0000000  0.0000000 1.00000000       FALSE
#>    300     0      0.00 0.0000000  0.0000000 1.00000000       FALSE
#> 
#> Over-represented: 2 | Under-represented: 0
plot(m)

```
