# Tidy table of a motif extraction

Tidy table of a motif extraction

## Usage

``` r
# S3 method for class 'cograph_motif_analysis'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  what = c("results", "types"),
  ...
)
```

## Arguments

- x:

  A `cograph_motif_analysis` object from
  [`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md).

- row.names, optional:

  Ignored; present for the generic.

- what:

  `"results"` (default) for the triad table, or `"types"` for one row
  per MAN type with columns `type` and `count` (the number of unit-level
  occurrences of that type).

- ...:

  Ignored.

## Value

A data frame: the `results` table described in
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md),
or the per-type counts.

## Examples

``` r
m <- extract_motifs(regulation_net, min_transitions = 0)
as.data.frame(m, what = "types")
#>   type count
#> 1 030T  8.37
#> 2 120C  3.52
#> 3 120D  1.92
#> 4 030C  1.34
#> 5 120U  1.19
```
