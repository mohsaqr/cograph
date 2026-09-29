# Motif Results as a Data Frame

Returns the tables held by a motif result from
[`motifs`](https://sonsoles.me/cograph/reference/motifs.md) or
[`subgraphs`](https://sonsoles.me/cograph/reference/subgraphs.md) as
tidy data frames.

## Usage

``` r
# S3 method for class 'cograph_motif_result'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  what = c("results", "types")
)
```

## Arguments

- x:

  A `cograph_motif_result` object.

- row.names, optional:

  Standard [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)
  arguments; `row.names` replaces the default row names.

- ...:

  Unused.

- what:

  Which table to return. `"results"` (default) returns the main table:
  one row per triad type for a census, or one row per node triple and
  type for
  [`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md).
  `"types"` returns one row per triad type with its `count`: the number
  of triads of that type in a census, or the number of node triples of
  that type in
  [`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md).

## Value

A `data.frame`. For `what = "results"` in a census, the columns are
`type` and `count`, plus `expected`, `z`, `p` and `sig` when
significance was tested. For
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md), the
columns are `triad`, `node1`, `node2`, `node3`, `type` and `observed`,
plus the significance columns when tested. For `what = "types"`, the
columns are `type` and `count`.

## See also

[`motifs`](https://sonsoles.me/cograph/reference/motifs.md),
[`subgraphs`](https://sonsoles.me/cograph/reference/subgraphs.md)

## Examples

``` r
census <- motifs(regulation_net, significance = FALSE)
as.data.frame(census)
#>   type count
#> 1 030T    11
#> 2 120C     3
#> 3 030C     2
#> 4 120D     2
#> 5 120U     1
as.data.frame(census, what = "types")
#>   type count
#> 1 030T    11
#> 2 120C     3
#> 3 030C     2
#> 4 120D     2
#> 5 120U     1
```
