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
#> 1 2.601976 5.727621 6.172122
#> 2 5.061220 2.021506 7.146157
#> 3 5.786749 5.892783 6.844560
#> 
#> $igraph_result
#>          A        D        G
#> A 0.000000 5.727621 6.172122
#> D 5.061220 0.000000 7.146157
#> G 5.786749 5.892783 0.000000
#> 
#> $matches
#> [1] TRUE
#> 
#> $difference
#> NULL
#> 
```
