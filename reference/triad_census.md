# Triad Census

Counts the 16 types of triads in a directed network using MAN notation.
Edge weights are ignored.

## Usage

``` r
triad_census(x)
```

## Arguments

- x:

  A matrix, igraph object, or cograph_network.

## Value

A named numeric vector of length 16 giving the count of each MAN triad
type, in the order listed under Details.

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
#>  003  012  102 021D 021U 021C 111D 111U 030T 030C  201 120D 120U 120C  210  300 
#>    7   27    2    9   11   29    9    7   11    2    0    2    1    3    0    0 
```
