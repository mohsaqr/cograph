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
nodes <- c("A", "B", "C")
t1 <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, dimnames = list(nodes, nodes))
t2 <- matrix(c(0, 1, 1, 1, 0, 0, 1, 0, 0), 3, 3, dimnames = list(nodes, nodes))
layers <- list(T1 = t1, T2 = t2)

layer_similarity_matrix(layers, "cosine")
#>     T1  T2
#> T1 1.0 0.5
#> T2 0.5 1.0
layer_similarity_matrix(layers, "jaccard")
#>           T1        T2
#> T1 1.0000000 0.3333333
#> T2 0.3333333 1.0000000
```
