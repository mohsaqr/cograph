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
#>  motif count null_mean   null_sd    z_score     p_value significant
#>    003     0      0.00 0.0000000  0.0000000 1.000000000       FALSE
#>    012     0      0.00 0.0000000  0.0000000 1.000000000       FALSE
#>    102     0      0.18 0.3861229 -0.4661728 0.641091822       FALSE
#>   021D     0      0.00 0.0000000  0.0000000 1.000000000       FALSE
#>   021U     0      0.79 0.8795660 -0.8981702 0.369094805       FALSE
#>   021C     0      0.36 0.6744994 -0.5337291 0.593528952       FALSE
#>   111D     0      0.10 0.3015113 -0.3316625 0.740144136       FALSE
#>   111U     2      0.22 0.6289321  2.8301943 0.004651974        TRUE
#>   030T     0      0.11 0.3144660 -0.3497993 0.726489324       FALSE
#>   030C     0      0.30 0.6113406 -0.4907248 0.623621117       FALSE
#>    201     0      0.00 0.0000000  0.0000000 1.000000000       FALSE
#>   120D     2      0.31 0.6620293  2.5527570 0.010687403        TRUE
#>   120U     0      0.10 0.3015113 -0.3316625 0.740144136       FALSE
#>   120C     0      0.06 0.2386833 -0.2513792 0.801520967       FALSE
#>    210     0      0.00 0.0000000  0.0000000 1.000000000       FALSE
#>    300     0      0.00 0.0000000  0.0000000 1.000000000       FALSE
#> 
#> Over-represented: 2 | Under-represented: 0
plot(m)

```
