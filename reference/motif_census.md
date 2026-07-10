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

- ...:

  Passed to methods; currently unused.

## Value

A `cograph_motifs` data frame with motif count, null-model mean,
null-model standard deviation, z-score, p-value, and significance
columns. The motif size, directed flag, null-model method, and number of
random networks are stored as attributes.

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
#>    102     0      0.19 0.3942772 -0.4818944 0.629880957       FALSE
#>   021D     0      0.00 0.0000000  0.0000000 1.000000000       FALSE
#>   021U     0      0.77 0.8628705 -0.8923703 0.372194475       FALSE
#>   021C     0      0.48 0.7847022 -0.6116970 0.540738243       FALSE
#>   111D     0      0.12 0.3265986 -0.3674235 0.713303174       FALSE
#>   111U     2      0.20 0.6030227  2.9849623 0.002836133        TRUE
#>   030T     0      0.06 0.2386833 -0.2513792 0.801520967       FALSE
#>   030C     0      0.44 0.7563869 -0.5817128 0.560760120       FALSE
#>    201     0      0.00 0.0000000  0.0000000 1.000000000       FALSE
#>   120D     2      0.23 0.6171783  2.8678907 0.004132182        TRUE
#>   120U     0      0.18 0.3861229 -0.4661728 0.641091822       FALSE
#>   120C     0      0.03 0.1714466 -0.1749816 0.861094100       FALSE
#>    210     0      0.00 0.0000000  0.0000000 1.000000000       FALSE
#>    300     0      0.00 0.0000000  0.0000000 1.000000000       FALSE
#> 
#> Over-represented: 2 | Under-represented: 0
plot(m)

```
