# Triad Census

Counts the 16 types of triads in a directed network using MAN notation.
An edge is a cell with a positive weight; the weight values are
otherwise ignored, so a zero or negative weight is no edge.

## Usage

``` r
triad_census(x)
```

## Arguments

- x:

  A matrix, igraph object, or cograph_network.

## Value

A data frame with one row per MAN triad type (16 rows), in the order
listed under Details, and the columns `type` (the MAN code) and `count`
(the number of triads of that type). The counts sum to `choose(n, 3)`
for `n` nodes.

## Details

The triad census is defined only for directed networks. Matrix input is
read as directed. An undirected igraph or cograph_network input raises
an error.

A MAN code gives the number of mutual (reciprocated) dyads, the number
of asymmetric dyads and the number of null (absent) dyads of a triad,
followed by a letter that separates types with the same counts. The 16
triad types, in the order of the result, are 003, 012, 102, 021D, 021U,
021C, 111D, 111U, 030T, 030C, 201, 120D, 120U, 120C, 210 and 300.

## See also

[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md) for the
unified API,
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md)

Other motifs:
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](https://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](https://sonsoles.me/cograph/reference/get_edge_list.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md),
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md)

## Examples

``` r
cograph::triad_census(regulation_net)
#>    type count
#> 1   003     7
#> 2   012    27
#> 3   102     2
#> 4  021D     9
#> 5  021U    11
#> 6  021C    29
#> 7  111D     9
#> 8  111U     7
#> 9  030T    11
#> 10 030C     2
#> 11  201     0
#> 12 120D     2
#> 13 120U     1
#> 14 120C     3
#> 15  210     0
#> 16  300     0
```
