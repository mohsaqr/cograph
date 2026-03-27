# Triad Census

Count the 16 types of triads in a directed network using MAN notation.

## Usage

``` r
triad_census(x)
```

## Arguments

- x:

  A matrix, igraph object, or cograph_network

## Value

Named vector of triad counts

## Details

Triad census is defined only for directed networks. The input is always
treated as directed.

MAN notation describes triads by:

- M: number of Mutual (reciprocal) edges

- A: number of Asymmetric edges

- N: number of Null (absent) edges

The 16 triad types are: 003, 012, 102, 021D, 021U, 021C, 111D, 111U,
030T, 030C, 201, 120D, 120U, 120C, 210, 300

## See also

[`motifs()`](http://sonsoles.me/cograph/reference/motifs.md) for the
unified API,
[`motif_census()`](http://sonsoles.me/cograph/reference/motif_census.md)

Other motifs:
[`extract_motifs()`](http://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](http://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](http://sonsoles.me/cograph/reference/get_edge_list.md),
[`motif_census()`](http://sonsoles.me/cograph/reference/motif_census.md),
[`motifs()`](http://sonsoles.me/cograph/reference/motifs.md),
[`plot.cograph_motif_analysis()`](http://sonsoles.me/cograph/reference/plot.cograph_motif_analysis.md),
[`plot.cograph_motifs()`](http://sonsoles.me/cograph/reference/plot.cograph_motifs.md),
[`subgraphs()`](http://sonsoles.me/cograph/reference/subgraphs.md)

## Examples

``` r
mat <- matrix(sample(0:1, 100, replace = TRUE), 10, 10)
diag(mat) <- 0
triad_census(mat)
#>  003  012  102 021D 021U 021C 111D 111U 030T 030C  201 120D 120U 120C  210  300 
#>    0    4    5    6    6   11   11   10    8    3   10    5   12   11   16    2 
```
