# Extract Specific Motif Instances (Subgraphs)

Convenience wrapper for `motifs(x, named_nodes = TRUE, ...)`. Returns
specific node triples forming each MAN pattern.

## Usage

``` r
subgraphs(...)
```

## Arguments

- ...:

  Additional arguments passed to plot helpers.

## Value

Same as [`motifs`](http://sonsoles.me/cograph/reference/motifs.md). See
that function for details.

## See also

[`motifs()`](http://sonsoles.me/cograph/reference/motifs.md)

Other motifs:
[`extract_motifs()`](http://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](http://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](http://sonsoles.me/cograph/reference/get_edge_list.md),
[`motif_census()`](http://sonsoles.me/cograph/reference/motif_census.md),
[`motifs()`](http://sonsoles.me/cograph/reference/motifs.md),
[`plot.cograph_motif_analysis()`](http://sonsoles.me/cograph/reference/plot.cograph_motif_analysis.md),
[`plot.cograph_motifs()`](http://sonsoles.me/cograph/reference/plot.cograph_motifs.md),
[`triad_census()`](http://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
mat <- matrix(c(0,3,2,0, 0,0,5,1, 0,0,0,4, 2,0,0,0), 4, 4, byrow = TRUE)
rownames(mat) <- colnames(mat) <- c("Plan","Execute","Monitor","Adapt")
subgraphs(mat, significance = FALSE)
#> No motifs with count > 5.
#> NULL
```
