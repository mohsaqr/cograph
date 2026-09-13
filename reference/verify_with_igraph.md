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

  Cluster specification (see
  [`csum`](https://sonsoles.me/cograph/reference/csum.md))

- method:

  Aggregation method. Default "sum".

- type:

  Normalization type. Defaults to "raw" for igraph compatibility.

## Value

A list with components `our_result` (cograph's macro weight matrix),
`igraph_result` (igraph's `contract()` +
[`simplify()`](https://sonsoles.me/cograph/reference/simplify.md)
matrix), `matches` (logical: do the off-diagonals agree to within
1e-10?) and `difference` (the
[`all.equal()`](https://rdrr.io/r/base/all.equal.html) report when they
do not, otherwise NULL). Returns `NULL` with a message if igraph is not
installed.

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
#> 1 3.216644 4.649900 5.903074
#> 2 4.144425 3.476482 6.006692
#> 3 6.027886 4.447357 7.180965
#> 
#> $igraph_result
#>          A        D        G
#> A 0.000000 4.649900 5.903074
#> D 4.144425 0.000000 6.006692
#> G 6.027886 4.447357 0.000000
#> 
#> $matches
#> [1] TRUE
#> 
#> $difference
#> NULL
#> 
```
