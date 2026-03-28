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

[`motifs()`](http://sonsoles.me/cograph/reference/motifs.md) for the
unified API,
[`extract_motifs()`](http://sonsoles.me/cograph/reference/extract_motifs.md)
for detailed triad extraction,
[`plot.cograph_motifs()`](http://sonsoles.me/cograph/reference/plot.cograph_motifs.md)
for plotting

Other motifs:
[`extract_motifs()`](http://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](http://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](http://sonsoles.me/cograph/reference/get_edge_list.md),
[`motifs()`](http://sonsoles.me/cograph/reference/motifs.md),
[`plot.cograph_motif_analysis()`](http://sonsoles.me/cograph/reference/plot.cograph_motif_analysis.md),
[`plot.cograph_motifs()`](http://sonsoles.me/cograph/reference/plot.cograph_motifs.md),
[`subgraphs()`](http://sonsoles.me/cograph/reference/subgraphs.md),
[`triad_census()`](http://sonsoles.me/cograph/reference/triad_census.md)

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
#> Size: 3-node motifs (directed)
#> Null model: configuration (n=100)
#> 
#> Significant motifs:
#>  motif count expected    z       p
#>   111U     2      0.1 4.87 1.1e-06
#>   120D     2      0.2 3.95 7.7e-05
#> 
#> Over-represented: 2 | Under-represented: 0
plot(m)

```
