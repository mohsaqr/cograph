# Layer Similarity

Computes similarity between two network layers.

## Usage

``` r
layer_similarity(
  A1,
  A2,
  method = c("jaccard", "overlap", "hamming", "cosine", "pearson")
)

lsim(A1, A2, method = c("jaccard", "overlap", "hamming", "cosine", "pearson"))
```

## Arguments

- A1:

  First adjacency matrix

- A2:

  Second adjacency matrix, with the same dimensions as `A1`

- method:

  Comparison method: "jaccard" (default), "overlap", "hamming", "cosine"
  or "pearson"

## Value

A single numeric value. All methods except `"hamming"` return a
similarity, where higher values mean more alike layers. `"hamming"`
returns a distance, the number of matrix cells whose edge presence
differs between the two layers. Lower values then mean more alike
layers, and the value is not bounded by 1. `NA` is returned when the
denominator is undefined (`"jaccard"` with no edges in either layer,
`"overlap"` with an empty layer, `"cosine"` with an all-zero layer).

## Details

`"jaccard"`, `"overlap"` and `"hamming"` compare edge presence (`A > 0`)
and ignore weights. `"cosine"` and `"pearson"` are computed on the cell
values, diagonal included. Matrices of different dimensions raise an
error.

## Examples

``` r
layer_similarity(regulation_net, t(regulation_net), method = "cosine")
#> [1] 0.1197053
```
