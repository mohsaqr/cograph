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

  Number of random networks for the null model. Must be a whole number
  of at least 2. Default 100.

- method:

  Null model method: "configuration" (preserves degree) or "gnm"
  (preserves edge count). Default "configuration".

- directed:

  Logical. Treat as directed? Default auto-detected.

- seed:

  Random seed for reproducibility. Default NULL. When supplied, the
  caller's RNG state is saved and restored.

- ...:

  Passed to methods; currently unused.

## Value

A `cograph_motifs` data frame with one row per motif class and columns:

- motif:

  Motif class name (the 16 MAN codes for directed triads, the four
  undirected triad classes, or `motif_<i>` labels for size 4).

- count:

  Observed number of that motif in the network.

- null_mean, null_sd:

  Mean and standard deviation of the count across the `n_random` null
  graphs.

- z_score:

  `(count - null_mean) / null_sd`; `NA` when the null is degenerate
  (`null_sd = 0`) and the observation differs from it.

- p_value:

  Two-sided empirical (add-one corrected) permutation p-value, not a
  Gaussian approximation.

- significant:

  Logical, `p_value < 0.05`.

The motif size (`"size"`), directed flag (`"directed"`), null-model
method (`"method"`), and number of random networks (`"n_random"`) are
stored as attributes. Self-loops and multiple edges are removed before
counting.

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
#>  motif count null_mean  null_sd    z_score p_value significant
#>    003     0      0.00 0.000000  0.0000000       1       FALSE
#>    012     0      0.00 0.000000  0.0000000       1       FALSE
#>    102     0      0.00 0.000000  0.0000000       1       FALSE
#>   021D     0      0.00 0.000000  0.0000000       1       FALSE
#>   021U     0      0.00 0.000000  0.0000000       1       FALSE
#>   021C     0      0.00 0.000000  0.0000000       1       FALSE
#>   111D     0      0.98 1.004837 -0.9752828       1       FALSE
#>   111U     0      0.98 1.004837 -0.9752828       1       FALSE
#>   030T     2      1.02 1.004837  0.9752828       1       FALSE
#>   030C     2      1.02 1.004837  0.9752828       1       FALSE
#>    201     0      0.00 0.000000  0.0000000       1       FALSE
#>   120D     0      0.00 0.000000  0.0000000       1       FALSE
#>   120U     0      0.00 0.000000  0.0000000       1       FALSE
#>   120C     0      0.00 0.000000  0.0000000       1       FALSE
#>    210     0      0.00 0.000000  0.0000000       1       FALSE
#>    300     0      0.00 0.000000  0.0000000       1       FALSE
#> 
#> Over-represented: 0 | Under-represented: 0
plot(m)
#> No motifs to plot. Try show_nonsig = TRUE
```
