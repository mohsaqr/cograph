# Convert a group_tna to mcml

Two modes depending on whether `clusters` is provided:

- With clusters (row-level):

  For group_tna from `tna::group_model(cluster_data(...))`. `clusters`
  is the row-to-group assignments. Per-cluster tnas are taken as-is.
  Macro data is the assignments vector.

- Without clusters (node-level):

  For group_tna from `as_tna(cluster_summary(...))`. Cluster membership
  inferred from each tna's labels. Macro rebuilt from original data.

## Usage

``` r
.group_tna_to_mcml(
  x,
  clusters = NULL,
  method = "sum",
  type = "tna",
  directed = TRUE,
  compute_within = TRUE
)
```
