# Verify Against igraph

Confirms numerical match with igraph's contract_vertices + simplify.

## Usage

``` r
verify_with_igraph(x, clusters, method = "sum", type = "raw")

verify_igraph(x, clusters, method = "sum", type = "raw")
```

## Arguments

- x:

  Adjacency matrix

- clusters:

  Cluster specification

- method:

  Aggregation method

- type:

  Normalization type. Defaults to "raw" for igraph compatibility.

## Value

List with comparison results

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE)) {
  mat <- matrix(runif(100), 10, 10)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- LETTERS[1:10]
  clusters <- c(1,1,1,2,2,2,3,3,3,3)
  verify_igraph(mat, clusters)
}
#> $our_result
#>          1        2        3
#> 1 3.384887 4.322376 6.592236
#> 2 5.539816 2.510548 5.899448
#> 3 6.807112 5.960876 5.690940
#> 
#> $igraph_result
#>          A        D        G
#> A 0.000000 4.322376 6.592236
#> D 5.539816 0.000000 5.899448
#> G 6.807112 5.960876 0.000000
#> 
#> $matches
#> [1] TRUE
#> 
#> $difference
#> NULL
#> 
```
