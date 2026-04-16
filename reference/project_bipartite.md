# Project Bipartite Network to One-Mode

Projects a two-mode (bipartite/incidence) network into a one-mode
adjacency matrix. Row-mode projection yields a matrix of shared-column
connections among row nodes; column-mode projection does the converse.

## Usage

``` r
project_bipartite(x, mode = "rows", method = "sum", ...)
```

## Arguments

- x:

  An incidence matrix (rows = type 1 nodes, columns = type 2 nodes)
  where non-zero entries indicate connections. Can also be a data.frame
  with columns `type1`, `type2`, and optionally `weight`.

- mode:

  Character. `"rows"` (default) projects onto row nodes (result: n_rows
  x n_rows). `"columns"` projects onto column nodes (result: n_cols x
  n_cols).

- method:

  Character. Projection method:

  `"sum"`

  :   Weighted projection: `A %*% t(A)` (rows) or `t(A) %*% A`
      (columns). Edge weight equals sum of shared connection weights.

  `"binary"`

  :   Co-occurrence count: binarize A first, then compute overlap. Edge
      weight equals number of shared connections.

  `"jaccard"`

  :   Jaccard similarity: shared / (total_i + total_j - shared) for each
      pair.

  `"cosine"`

  :   Cosine similarity: dot product of row (or column) vectors divided
      by the product of their norms.

  `"newman"`

  :   Newman's weighted projection (Newman 2001): each shared
      affiliation contributes `1 / (d_k - 1)` where `d_k` is the degree
      of the shared node. Gives more weight to connections through
      exclusive affiliations.

- ...:

  Additional arguments (currently unused).

## Value

A square adjacency matrix with row and column names preserved from the
input. Diagonal is set to 0 (no self-loops).

## Details

For the Newman projection, affiliations shared with only one node of the
focal type (`d_k = 1`) are skipped, since `1 / (d_k - 1)` is undefined.
This follows the convention in Newman (2001).

## References

Newman, M. E. J. (2001). Scientific collaboration networks. II. Shortest
paths, weighted networks, and centrality. *Physical Review E*, 64(1),
016132.

## See also

[`is_bipartite`](https://sonsoles.me/cograph/reference/is_bipartite.md),
[`plot_heatmap`](https://sonsoles.me/cograph/reference/plot_heatmap.md)

## Examples

``` r
# Incidence matrix: 4 students x 3 courses
inc <- matrix(c(1, 1, 0,
                1, 0, 1,
                0, 1, 1,
                1, 1, 1), 4, 3, byrow = TRUE)
rownames(inc) <- paste0("S", 1:4)
colnames(inc) <- paste0("C", 1:3)

# Student co-enrollment (weighted)
cograph::project_bipartite(inc, mode = "rows", method = "sum")
#>    S1 S2 S3 S4
#> S1  0  1  1  2
#> S2  1  0  1  2
#> S3  1  1  0  2
#> S4  2  2  2  0

# Course overlap (Jaccard similarity)
cograph::project_bipartite(inc, mode = "columns", method = "jaccard")
#>     C1  C2  C3
#> C1 0.0 0.5 0.5
#> C2 0.5 0.0 0.5
#> C3 0.5 0.5 0.0

# Newman's weighted projection
cograph::project_bipartite(inc, mode = "rows", method = "newman")
#>     S1  S2  S3 S4
#> S1 0.0 0.5 0.5  1
#> S2 0.5 0.0 0.5  1
#> S3 0.5 0.5 0.0  1
#> S4 1.0 1.0 1.0  0
```
