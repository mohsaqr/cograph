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
  seed = NULL
)

csig(
  x,
  communities,
  n_random = 100,
  method = c("configuration", "gnm"),
  seed = NULL
)
```

## Arguments

- x:

  Network input: adjacency matrix, igraph object, or cograph_network.

- communities:

  A communities object (from
  [`communities`](http://sonsoles.me/cograph/reference/communities.md)
  or igraph) or a membership vector (integer vector where
  `communities[i]` is the community of node i).

- n_random:

  Number of random networks to generate for the null distribution.
  Default 100.

- method:

  Null model type:

  "configuration"

  :   Preserves degree sequence (default). More stringent test.

  "gnm"

  :   Erdos-Renyi model with same number of edges. Tests against random
      baseline.

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

  Standardized score: (observed - null_mean) / null_sd

- p_value:

  One-sided p-value (probability of observing equal or higher modularity
  by chance)

- null_values:

  Vector of modularity values from null distribution

- method:

  Null model method used

- n_random:

  Number of random networks generated

See `cluster_significance`.

## Details

The test works by:

1.  Computing the modularity of the provided community structure

2.  Generating `n_random` random networks using the specified null model

3.  For each random network, detecting communities with Louvain and
    computing modularity

4.  Comparing the observed modularity to this null distribution

A significant result (low p-value) indicates that the community
structure is stronger than expected by chance for networks with similar
properties.

## References

Reichardt, J., & Bornholdt, S. (2006). Statistical mechanics of
community detection. *Physical Review E*, 74, 016110.

## See also

[`communities`](http://sonsoles.me/cograph/reference/communities.md),
[`cluster_quality`](http://sonsoles.me/cograph/reference/cluster_quality.md)

## Examples

``` r
if (FALSE) { # \dontrun{
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::make_graph("Zachary")
  comm <- community_louvain(g)

  # Test significance
  sig <- cluster_significance(g, comm, n_random = 100, seed = 123)
  print(sig)

  # Configuration model (stricter test)
  sig2 <- cluster_significance(g, comm, method = "configuration")
}
} # }
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::make_graph("Zachary")
  comm <- community_louvain(g)
  csig(g, comm, n_random = 20, seed = 1)
}
#> Cluster Significance Test
#> =========================
#> 
#>   Null model:           configuration (n = 20 )
#>   Observed modularity:  0.4156 
#>   Null mean:            0.3866 
#>   Null SD:              0.0189 
#>   Z-score:              1.54 
#>   P-value:              0.062349 
#> 
#>   Conclusion: No significant community structure (p >= 0.05)
```
