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

  Similarity method: "jaccard", "overlap", "hamming", "cosine",
  "pearson"

## Value

Numeric similarity value

## Examples

``` r
A1 <- matrix(c(0,1,1,0, 1,0,0,1, 1,0,0,1, 0,1,1,0), 4, 4)
A2 <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), 4, 4)

layer_similarity(A1, A2, "jaccard")  # Edge overlap
#> [1] 0.4
layer_similarity(A1, A2, "cosine")   # Weight similarity
#> [1] 0.5773503
```
