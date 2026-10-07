# Verify Against igraph

Compares the macro weights of
[`csum`](https://sonsoles.me/cograph/reference/csum.md) with the result
of contracting the clusters in igraph
([`igraph::contract()`](https://r.igraph.org/reference/contract.html)
followed by
[`igraph::simplify()`](https://r.igraph.org/reference/simplify.html)).

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

  Normalization type passed to
  [`csum`](https://sonsoles.me/cograph/reference/csum.md). Default
  "raw", the only type whose values igraph reproduces.

## Value

A list with components `our_result` (cograph's macro weight matrix),
`igraph_result` (igraph's `contract()` +
[`simplify()`](https://sonsoles.me/cograph/reference/simplify.md)
matrix, diagonal set to zero), `matches` (logical, whether the
off-diagonal cells agree to within 1e-10) and `difference` (the
[`all.equal()`](https://rdrr.io/r/base/all.equal.html) report when they
do not, otherwise NULL). Returns `NULL` with a message if igraph is not
installed.

## Examples

``` r
clusters <- list(C1 = c("Explore", "Reflect", "Discuss"),
                 C2 = c("Plan", "Create", "Share"),
                 C3 = c("Monitor", "Adapt", "Synthesize", "Evaluate"))
verify_with_igraph(regulation_net, clusters = clusters)
#> $our_result
#>      C1   C2   C3
#> C1 1.05 0.41 0.15
#> C2 0.54 1.00 2.06
#> C3 1.11 0.48 1.16
#> 
#> $igraph_result
#>         Explore Plan Monitor
#> Explore    0.00 0.41    0.15
#> Plan       0.54 0.00    2.06
#> Monitor    1.11 0.48    0.00
#> 
#> $matches
#> [1] TRUE
#> 
#> $difference
#> NULL
#> 
```
