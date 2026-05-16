# Extract Specific Motif Instances (Subgraphs)

Convenience wrapper for `motifs(x, named_nodes = TRUE, ...)`. Returns
specific node triples forming each MAN pattern.

## Usage

``` r
subgraphs(...)
```

## Arguments

- ...:

  Arguments passed to
  [`motifs()`](https://sonsoles.me/cograph/reference/motifs.md).

## Value

A `cograph_motif_result` object with `named_nodes = TRUE`. Contains
`$results` (data frame with columns `triad`, `type`, `observed`, and
optionally `z`, `p`, `sig`), `$type_summary`, `$level`, `$n_units`, and
`$params`.

## See also

[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md)

Other motifs:
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](https://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](https://sonsoles.me/cograph/reference/get_edge_list.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md),
[`plot.cograph_motif_analysis()`](https://sonsoles.me/cograph/reference/plot.cograph_motif_analysis.md),
[`plot.cograph_motifs()`](https://sonsoles.me/cograph/reference/plot.cograph_motifs.md),
[`triad_census()`](https://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
mat <- matrix(c(0,3,2,0, 0,0,5,1, 0,0,0,4, 2,0,0,0), 4, 4, byrow = TRUE)
rownames(mat) <- colnames(mat) <- c("Plan","Execute","Monitor","Adapt")
subgraphs(mat, significance = FALSE)
#> Showing triangle patterns (count >= 5). For all MAN types use pattern = 'all'.
#> Motif Subgraphs 
#> Level: aggregate | States: 4 | Pattern: triangle 
#> Min count: >= 5 
#> 
#> Type distribution:
#> 
#> 030C 030T 
#>    2    2 
#> 
#> Top 4 results:
#>                      triad type observed
#>   Plan - Execute - Monitor 030T       10
#>  Execute - Monitor - Adapt 030T       10
#>     Plan - Monitor - Adapt 030C        8
#>     Plan - Execute - Adapt 030C        6
```
