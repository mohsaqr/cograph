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

  Second adjacency matrix

- method:

  Comparison method: "jaccard" (default), "overlap", "hamming", "cosine"
  or "pearson"

## Value

A single numeric value. All methods except `"hamming"` return a
similarity (higher = more alike); `"hamming"` returns a *distance* - the
number of matrix cells whose edge presence differs between the two
layers - so lower means more alike and the value is not bounded by 1.
`NA` is returned when the denominator is undefined (`"jaccard"` with no
edges in either layer, `"overlap"` with an empty layer, `"cosine"` with
an all-zero layer).

## Details

`"jaccard"`, `"overlap"` and `"hamming"` compare edge *presence*
(`A > 0`) and therefore ignore weights; `"cosine"` and `"pearson"` are
computed on the raw cell values. The two matrices must have identical
dimensions.

## Examples

``` r
A1 <- matrix(c(0,1,1,0, 1,0,0,1, 1,0,0,1, 0,1,1,0), 4, 4)
A2 <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), 4, 4)

layer_similarity(A1, A2, "jaccard")  # Edge overlap
#> [1] 0.4
layer_similarity(A1, A2, "cosine")   # Weight similarity
#> [1] 0.5773503
```
