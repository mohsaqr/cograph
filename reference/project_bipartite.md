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
      (columns). Edge weight equals sum of shared connection-weight
      products.

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

A square adjacency matrix, one row and column per node of the projected
mode: `n_rows x n_rows` named by `rownames(x)` for `mode = "rows"`,
`n_cols x n_cols` named by `colnames(x)` for `mode = "columns"`. The
diagonal is set to 0 (no self-loops).

## Details

Only `method = "sum"` and `method = "cosine"` use the incidence values
themselves. `"binary"`, `"jaccard"` and `"newman"` first binarize the
incidence matrix (`x > 0`), so any weights are discarded for those
three.

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
cograph::project_bipartite(regulation_net, mode = "rows", method = "jaccard")
#>              Explore      Plan   Monitor     Adapt   Reflect   Discuss
#> Explore    0.0000000 0.1666667 0.0000000 0.0000000 0.0000000 0.2500000
#> Plan       0.1666667 0.0000000 0.1666667 0.1428571 0.1666667 0.1428571
#> Monitor    0.0000000 0.1666667 0.0000000 0.0000000 0.0000000 0.2500000
#> Adapt      0.0000000 0.1428571 0.0000000 0.0000000 0.2500000 0.2000000
#> Reflect    0.0000000 0.1666667 0.0000000 0.2500000 0.0000000 0.2500000
#> Discuss    0.2500000 0.1428571 0.2500000 0.2000000 0.2500000 0.0000000
#> Synthesize 0.2500000 0.1428571 0.0000000 0.0000000 0.2500000 0.2000000
#> Evaluate   0.2500000 0.1428571 0.2500000 0.0000000 0.2500000 0.2000000
#> Create     0.2000000 0.5000000 0.0000000 0.1666667 0.5000000 0.1666667
#> Share      0.0000000 0.1428571 0.2500000 0.0000000 0.2500000 0.0000000
#>            Synthesize  Evaluate    Create     Share
#> Explore     0.2500000 0.2500000 0.2000000 0.0000000
#> Plan        0.1428571 0.1428571 0.5000000 0.1428571
#> Monitor     0.0000000 0.2500000 0.0000000 0.2500000
#> Adapt       0.0000000 0.0000000 0.1666667 0.0000000
#> Reflect     0.2500000 0.2500000 0.5000000 0.2500000
#> Discuss     0.2000000 0.2000000 0.1666667 0.0000000
#> Synthesize  0.0000000 0.5000000 0.1666667 0.5000000
#> Evaluate    0.5000000 0.0000000 0.1666667 0.5000000
#> Create      0.1666667 0.1666667 0.0000000 0.1666667
#> Share       0.5000000 0.5000000 0.1666667 0.0000000
```
