# Pairwise Layer Similarities

Computes the similarity of every pair of layers with
[`layer_similarity`](https://sonsoles.me/cograph/reference/layer_similarity.md).

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

  Named list of adjacency matrices (one per layer); at least two are
  required.

- method:

  Comparison method: "jaccard" (default), "overlap", "cosine" or
  "pearson". The `"hamming"` distance is not accepted.

## Value

A symmetric L x L matrix of pairwise similarities with 1 on the
diagonal. The dimnames are the layer names, or `"Layer1"`, `"Layer2"`,
... for an unnamed list.

## Examples

``` r
layers <- list(forward = regulation_net, backward = t(regulation_net))
layer_similarity_matrix(layers, method = "cosine")
#>            forward  backward
#> forward  1.0000000 0.1197053
#> backward 0.1197053 1.0000000
```
