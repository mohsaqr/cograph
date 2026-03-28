# Extract Raw Edge List from TNA Model

Extract individual-level transition counts as an edge list from a tna
object.

## Usage

``` r
get_edge_list(x, by_individual = TRUE, drop_zeros = TRUE)
```

## Arguments

- x:

  A tna object created by
  [`tna::tna()`](http://sonsoles.me/tna/reference/build_model.md)

- by_individual:

  Logical. If TRUE (default), returns edge list with individual IDs. If
  FALSE, aggregates across all individuals.

- drop_zeros:

  Logical. If TRUE (default), excludes edges with zero count.

## Value

A data frame with columns:

- id:

  Individual identifier (only if `by_individual = TRUE`)

- from:

  Source state label

- to:

  Target state label

- count:

  Number of transitions

## See also

[`extract_motifs()`](http://sonsoles.me/cograph/reference/extract_motifs.md)
for motif analysis using edge lists

Other motifs:
[`extract_motifs()`](http://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](http://sonsoles.me/cograph/reference/extract_triads.md),
[`motif_census()`](http://sonsoles.me/cograph/reference/motif_census.md),
[`motifs()`](http://sonsoles.me/cograph/reference/motifs.md),
[`plot.cograph_motif_analysis()`](http://sonsoles.me/cograph/reference/plot.cograph_motif_analysis.md),
[`plot.cograph_motifs()`](http://sonsoles.me/cograph/reference/plot.cograph_motifs.md),
[`subgraphs()`](http://sonsoles.me/cograph/reference/subgraphs.md),
[`triad_census()`](http://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
Mod <- tna::tna(tna::group_regulation)

# Get edge list by individual
edges <- get_edge_list(Mod)
head(edges)
#>   id      from        to count
#> 1  1 synthesis     adapt     1
#> 2  1     adapt consensus     1
#> 3  1  cohesion consensus     1
#> 4  1      plan consensus     1
#> 5  1 consensus   discuss     1
#> 6  1 consensus      plan     1

# Aggregate across individuals
agg_edges <- get_edge_list(Mod, by_individual = FALSE)
```
