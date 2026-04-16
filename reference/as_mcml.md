# Convert to mcml

Convert various objects to the `mcml` class – a clean, tna-independent
representation of a multilayer cluster network.

## Usage

``` r
as_mcml(x, ...)

# S3 method for class 'cluster_summary'
as_mcml(x, ...)

# S3 method for class 'group_tna'
as_mcml(x, clusters = NULL, method = "sum", type = "tna", directed = TRUE, ...)

# S3 method for class 'mcml'
as_mcml(x, ...)

# Default S3 method
as_mcml(x, ...)
```

## Arguments

- x:

  Object to convert.

- ...:

  Additional arguments passed to methods.

- clusters:

  Integer or character vector of row-to-group assignments. Required when
  the `group_tna` has the same labels across all groups (row-level
  clustering from `tna::group_model(cluster_data(...))`).

- method:

  Aggregation method for macro weights (default `"sum"`).

- type:

  Transition type (default `"tna"`).

- directed:

  Logical; whether the network is directed (default `TRUE`).

## Value

An `mcml` object with components `macro`, `clusters`, `cluster_members`,
and `meta`.

An `mcml` object.

An `mcml` object. When `clusters` is provided, `macro$data` contains the
cluster assignments and `macro$weights` is `NULL` (the macro is the
sequence of clusters, not a summary).

The input `mcml` object unchanged.

## See also

[`build_mcml`](https://sonsoles.me/cograph/reference/build_mcml.md),
[`as_tna`](https://sonsoles.me/cograph/reference/as_tna.md)

## Examples

``` r
# From cluster_summary
mat <- matrix(c(0.5, 0.2, 0.3,
                0.1, 0.6, 0.3,
                0.4, 0.1, 0.5), 3, 3, byrow = TRUE,
              dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
clusters <- list(G1 = c("A", "B"), G2 = c("C"))
cs <- cluster_summary(mat, clusters, type = "tna")
m <- as_mcml(cs)
m$macro$weights
#>     G1  G2
#> G1 0.7 0.3
#> G2 0.5 0.5

if (FALSE) { # \dontrun{
# From group_tna with cluster assignments (requires tna + Nestimate)
seqs <- data.frame(T1 = c("A", "B", "A"), T2 = c("B", "A", "B"))
mod <- tna::tna(seqs)
cl <- Nestimate::cluster_data(mod, k = 2)
gt <- tna::group_model(cl)
m <- as_mcml(gt, clusters = cl$assignments)
} # }
```
