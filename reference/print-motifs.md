# Print motif results

Print methods for the results of
[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md) and
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md)
(`cograph_motif_result`),
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md)
(`cograph_motif_analysis`) and
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md)
(`cograph_motifs`). Each shows the analysis settings, the number of
units analysed where there are several, and the leading rows of the
results table.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the full table.

## Usage

``` r
# S3 method for class 'cograph_motif_result'
print(x, ...)

# S3 method for class 'cograph_motif_analysis'
print(x, n = 20, ...)

# S3 method for class 'cograph_motifs'
print(x, ...)
```

## Arguments

- x:

  A motif result.

- ...:

  Passed to
  [`print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html)
  for `cograph_motifs`; ignored otherwise.

- n:

  Number of triad rows to show. Default 20.

## Value

`x`, invisibly.

## Examples

``` r
print(extract_motifs(regulation_net, min_transitions = 0), n = 5)
#> Motif Analysis
#> Pattern: triangle | Edge method: any
#> Aggregate network | States: 10 | Triads: 19 shown of 19
#> 
#> Type distribution:
#> 030T 120C 120D 030C 120U 
#> 8.37 3.52 1.92 1.34 1.19 
#> 
#> Top 5 triads:
#>                         triad type observed
#> 1 Monitor - Evaluate - Create 120C     1.26
#> 2    Monitor - Create - Share 120C     1.26
#> 3      Plan - Monitor - Share 120U     1.19
#> 4    Plan - Evaluate - Create 030T     1.08
#> 5 Explore - Reflect - Discuss 120D     1.05
```
