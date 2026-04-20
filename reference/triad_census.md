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

[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md) for the
unified API,
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md)

Other motifs:
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](https://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](https://sonsoles.me/cograph/reference/get_edge_list.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md),
[`plot.cograph_motif_analysis()`](https://sonsoles.me/cograph/reference/plot.cograph_motif_analysis.md),
[`plot.cograph_motifs()`](https://sonsoles.me/cograph/reference/plot.cograph_motifs.md),
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md)

## Examples

``` r
mat <- matrix(sample(0:1, 100, replace = TRUE), 10, 10)
diag(mat) <- 0
triad_census(mat)
#>  003  012  102 021D 021U 021C 111D 111U 030T 030C  201 120D 120U 120C  210  300 
#>    1    3    2    4    5    9   11   17   16    2    5    6    7   10   17    5 
```
