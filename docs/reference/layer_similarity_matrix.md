# Pairwise Layer Similarities

Computes similarity matrix for all pairs of layers.

## Usage

``` r
layer_similarity_matrix(
  layers,
  method = c("jaccard", "overlap", "cosine", "pearson")
)

lsim_matrix(layers, method = c("jaccard", "overlap", "cosine", "pearson"))
```

## Arguments

- layers:

  List of adjacency matrices (one per layer)

- method:

  Similarity method

## Value

Symmetric matrix of pairwise similarities

## Examples

``` r
# layers <- list(T1 = mat1, T2 = mat2, T3 = mat3)
# layer_similarity_matrix(layers, "cosine")
```
