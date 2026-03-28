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
#> 1 2.529446 2.641382 4.980808
#> 2 5.265176 3.806871 5.784825
#> 3 5.087139 6.475312 4.371680
#> 
#> $igraph_result
#>          A        D        G
#> A 0.000000 2.641382 4.980808
#> D 5.265176 0.000000 5.784825
#> G 5.087139 6.475312 0.000000
#> 
#> $matches
#> [1] TRUE
#> 
#> $difference
#> NULL
#> 
```
