# Test Significance of Community Structure

Compares observed modularity against a null model distribution to assess
whether the detected community structure is statistically significant.

## Usage

``` r
cluster_significance(
  x,
  communities,
  n_random = 100,
  method = c("configuration", "gnm"),
  null = c("detect", "fixed"),
  seed = NULL
)

csig(
  x,
  communities,
  n_random = 100,
  method = c("configuration", "gnm"),
  null = c("detect", "fixed"),
  seed = NULL
)
```

## Arguments

- x:

  Network input: adjacency matrix, igraph object, or cograph_network.

- communities:

  A communities object (from
  [`communities`](https://sonsoles.me/cograph/reference/communities.md)
  or igraph) or a membership vector (integer vector where
  `communities[i]` is the community of node i).

- n_random:

  Number of random networks to generate for the null distribution.
  Default 100.

- method:

  Null model type:

  "configuration"

  :   (default) Undirected configuration model that preserves the total
      degree of each node.

  "gnm"

  :   Erdos-Renyi G(n, m) model with the same number of nodes and edges
      and the same directedness.

- null:

  Which null question to answer. Default `"detect"`:

  "detect"

  :   The null value is the modularity of the partition found by
      community detection on each null graph.

  "fixed"

  :   The null value is the modularity of the supplied `communities`
      membership evaluated on each null graph.

- seed:

  Random seed for reproducibility. Default NULL.

## Value

A `cograph_cluster_significance` object with:

- observed_modularity:

  Modularity of the input communities

- null_mean:

  Mean modularity of random networks

- null_sd:

  Standard deviation of null modularity

- z_score:

  Standardized score (observed - null_mean) / null_sd, or `NA` when
  `null_sd` is zero

- p_value:

  One-sided upper-tail p-value of `z_score` under the standard normal
  distribution; `NA` when `null_sd` is zero

- null_values:

  Vector of modularity values from null distribution

- method:

  Null model method used

- null:

  Which null question was asked ("detect" or "fixed")

- n_random:

  Number of random networks generated

## Details

The function generates `n_random` random networks from the null model.
With `null = "detect"`, community detection (Louvain, or fast greedy
when Louvain fails) is run on each null network and its modularity is
recorded. A low p-value then indicates that the observed partition is
stronger than the partitions detection recovers on random networks. With
`null = "fixed"`, the supplied membership is evaluated on each null
network. A low p-value then indicates that the partition explains more
structure in the observed network than in random networks, independently
of any detection algorithm.

The observed modularity is computed with the edge weights of `x`. When
`x` is weighted, each null network receives the observed edge weights,
randomly reassigned to its edges, so the observed and null modularity
are on the same scale.

## Printing and plotting

Printing the result shows the null model, the observed and null
modularity, the z-score and the p-value.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the result
plots a histogram of the null modularity values with the observed value
marked.

## References

Reichardt, J., & Bornholdt, S. (2006). Statistical mechanics of
community detection. *Physical Review E*, 74, 016110.

## See also

[`communities`](https://sonsoles.me/cograph/reference/communities.md),
[`cluster_quality`](https://sonsoles.me/cograph/reference/cluster_quality.md)

## Examples

``` r
comm <- communities(regulation_net, method = "walktrap")
cluster_significance(regulation_net, comm, n_random = 20, seed = 1)
#> Cluster Significance Test
#> =========================
#> 
#>   Null model:           configuration (n = 20 )
#>   Observed modularity:  0.2033 
#>   Null mean:            0.2828 
#>   Null SD:              0.0702 
#>   Z-score:              -1.13 
#>   P-value:              0.87136 
#> 
#>   Conclusion: No significant community structure (p >= 0.05)
```
