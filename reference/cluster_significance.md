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

  :   Preserves degree sequence (default). More stringent test.

  "gnm"

  :   Erdos-Renyi model with same number of edges. Tests against random
      baseline.

- null:

  Which null question to answer. Default `"detect"`:

  "detect"

  :   Null is the modularity of the *best partition found by community
      detection* on each null graph. Answers "is the observed partition
      stronger than what community detection would recover on similar
      random graphs?" — the historical behavior.

  "fixed"

  :   Null is the modularity of the supplied `communities` membership
      *evaluated on each null graph*. Answers "does the supplied
      partition itself explain more structure than it would on similar
      random graphs?" — the conservative test.

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

- null:

  Which null question was asked ("detect" or "fixed")

- n_random:

  Number of random networks generated

See `cluster_significance`.

## Details

Two null models are supported. The default, `null = "detect"`, generates
`n_random` random networks, runs community detection (Louvain, with
fast-greedy fallback) on each, and records the resulting modularity. Low
p-value means the observed partition beats what detection would return
on similar random graphs. `null = "fixed"` instead evaluates the
user-supplied membership on each null graph, so low p-value means the
partition itself is stronger than it would be on similar random graphs —
a tighter question that isolates the partition's quality from any
detector's behavior.

A significant result (low p-value) indicates that the community
structure is stronger than expected by chance for networks with similar
properties.

## References

Reichardt, J., & Bornholdt, S. (2006). Statistical mechanics of
community detection. *Physical Review E*, 74, 016110.

## See also

[`communities`](https://sonsoles.me/cograph/reference/communities.md),
[`cluster_quality`](https://sonsoles.me/cograph/reference/cluster_quality.md)

## Examples

``` r
g <- igraph::make_graph("Zachary")
comm <- community_louvain(g)
sig <- cluster_significance(g, comm, n_random = 20, seed = 123)
print(sig)
#> Cluster Significance Test
#> =========================
#> 
#>   Null model:           configuration (n = 20 )
#>   Observed modularity:  0.4156 
#>   Null mean:            0.3776 
#>   Null SD:              0.031 
#>   Z-score:              1.23 
#>   P-value:              0.10984 
#> 
#>   Conclusion: No significant community structure (p >= 0.05)
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::make_graph("Zachary")
  comm <- community_louvain(g)
  csig(g, comm, n_random = 20, seed = 1)
}
#> Cluster Significance Test
#> =========================
#> 
#>   Null model:           configuration (n = 20 )
#>   Observed modularity:  0.4151 
#>   Null mean:            0.3866 
#>   Null SD:              0.0189 
#>   Z-score:              1.51 
#>   P-value:              0.065619 
#> 
#>   Conclusion: No significant community structure (p >= 0.05)
```
