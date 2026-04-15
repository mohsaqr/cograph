# Centralization index

Computes Freeman's centralization for degree, betweenness, closeness, or
eigenvector centrality.

## Usage

``` r
centralization(
  x,
  measure = c("degree", "betweenness", "closeness", "eigenvector"),
  directed = NULL,
  mode = "all",
  ...
)
```

## Arguments

- x:

  Network input

- measure:

  One of "degree", "betweenness", "closeness", "eigenvector"

- directed:

  Logical or NULL

- mode:

  "all", "in", or "out"

- ...:

  Additional arguments passed to to_igraph()

## Value

Numeric scalar in \\\[0, 1\]\\

## Examples

``` r
star <- matrix(0, 5, 5)
star[1, 2:5] <- 1; star[2:5, 1] <- 1
cograph::centralization(star, "degree")
#> [1] 1
```
