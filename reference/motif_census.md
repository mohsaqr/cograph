# Network Motif Analysis

Counts the subgraph classes (motifs) of size 3 or 4 in a network and
tests their frequencies against random networks from a null model. Edge
weights are ignored, and self-loops and multiple edges are removed
before counting.

## Usage

``` r
motif_census(
  x,
  size = 3,
  n_random = 100,
  method = c("configuration", "gnm"),
  directed = NULL,
  seed = NULL
)
```

## Arguments

- x:

  A matrix, igraph object, or cograph_network.

- size:

  Motif size: 3 (triads) or 4 (tetrads). Default 3.

- n_random:

  Number of random networks for the null model. Must be a whole number
  of at least 2. Default 100.

- method:

  Null model method. `"configuration"` (default) rewires the graph with
  degree-preserving edge swaps. `"gnm"` draws random graphs with the
  same numbers of nodes and edges.

- directed:

  Logical or NULL. Whether the network is treated as directed. NULL
  (default) treats a matrix as directed when it is not symmetric and
  takes the directedness of an igraph or cograph_network input. A value
  that conflicts with an igraph or cograph_network input raises an
  error.

- seed:

  Random seed for reproducibility. Default NULL. When supplied, the
  caller's RNG state is saved and restored.

## Value

A `cograph_motifs` data frame with one row per motif class and columns:

- motif:

  Motif class name. Directed triads use the 16 MAN codes, undirected
  triads the classes `empty`, `edge`, `wedge` and `triangle`, and size 4
  the igraph isomorphism class labels `M1` to `M218` (directed) or `M1`
  to `M11` (undirected).

- count:

  Observed number of that motif in the network.

- null_mean, null_sd:

  Mean and standard deviation of the count across the `n_random` null
  graphs.

- z_score:

  `(count - null_mean) / null_sd`. When `null_sd = 0`, it is 0 if the
  count equals the null mean and `NA` otherwise.

- p_value:

  Two-sided empirical permutation p-value with add-one correction, based
  on the absolute deviation from the null mean.

- significant:

  Logical, `p_value < 0.05`.

The motif size (`"size"`), directed flag (`"directed"`), null-model
method (`"method"`), and number of random networks (`"n_random"`) are
stored as attributes.

## Details

Printing the result shows the motif table with the null-model settings
and the number of over- and under-represented motifs. The result is a
data frame and serves as the tidy table directly.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the result
is documented in
[`plot-results`](https://sonsoles.me/cograph/reference/plot-results.md).

## See also

[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md) for the
unified API,
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md)
for detailed triad extraction,
[`plot-results`](https://sonsoles.me/cograph/reference/plot-results.md)
for plotting

Other motifs:
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](https://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](https://sonsoles.me/cograph/reference/get_edge_list.md),
[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md),
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`triad_census()`](https://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
motif_census(regulation_net, n_random = 20, seed = 1)
#> Network Motif Analysis
#> Size: 3-node motifs (directed) | Null: configuration (n=20)
#> 
#>  motif count null_mean  null_sd     z_score   p_value significant
#>    003     7      6.90 2.149663  0.04651891 1.0000000       FALSE
#>    012    27     31.00 4.316431 -0.92669146 0.3809524       FALSE
#>    102     2      8.20 3.721912 -1.66581032 0.1428571       FALSE
#>   021D     9      8.05 2.305029  0.41214235 0.9047619       FALSE
#>   021U    11     10.25 2.788605  0.26895172 0.8571429       FALSE
#>   021C    29     19.70 4.910783  1.89379169 0.1428571       FALSE
#>   111D     9      9.05 2.910507 -0.01717914 1.0000000       FALSE
#>   111U     7      6.20 2.587419  0.30918843 0.8571429       FALSE
#>   030T    11      9.35 3.183427  0.51830928 0.5714286       FALSE
#>   030C     2      3.00 1.521772 -0.65712874 0.7142857       FALSE
#>    201     0      1.25 1.332785 -0.93788572 0.5714286       FALSE
#>   120D     2      1.45 1.190975  0.46180657 0.6190476       FALSE
#>   120U     1      1.50 1.235442 -0.40471361 1.0000000       FALSE
#>   120C     3      3.50 1.147079 -0.43588989 1.0000000       FALSE
#>    210     0      0.60 0.680557 -0.88163072 0.6190476       FALSE
#>    300     0      0.00 0.000000  0.00000000 1.0000000       FALSE
#> 
#> Over-represented: 0 | Under-represented: 0
```
