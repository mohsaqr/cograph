# Local Transitivity (Clustering Coefficient)

Proportion of triangles around each node relative to the number of
possible triangles. Measures how tightly clustered a node's neighborhood
is.

## Usage

``` r
centrality_transitivity(x, transitivity_type = "local", isolates = "nan", ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- transitivity_type:

  Type of transitivity: `"local"` (default), `"global"`, `"undirected"`,
  `"localundirected"`, `"barrat"` (weighted), `"weighted"`, or
  `"onnela"`. `"onnela"` computes the Onnela / Holme weighted clustering
  coefficient on the symmetrized matrix and matches
  `tna::centralities(., "Clustering")` byte-for-byte. Auto-set to
  `"onnela"` when `tna_network = TRUE` (passed via `...`) and the user
  did not pass an explicit value.

- isolates:

  How to handle isolate nodes: `"nan"` (default) or `"zero"`.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `weighted`, `directed`).

## Value

Named numeric vector of transitivity values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_transitivity(adj)
#> A B C 
#> 1 1 1 
```
