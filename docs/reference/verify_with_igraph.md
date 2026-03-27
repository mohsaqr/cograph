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
if (FALSE) { # \dontrun{
mat <- matrix(runif(100), 10, 10)
diag(mat) <- 0
rownames(mat) <- colnames(mat) <- LETTERS[1:10]
clusters <- c(1,1,1,2,2,2,3,3,3,3)
verify_igraph(mat, clusters)
} # }
```
