# Convert cluster_summary to tna Objects

Converts a `cluster_summary` or `mcml` object to tna models that can be
used with the functions of the tna package. The result holds a macro
(cluster-level) model and one model of the internal transitions of each
cluster, as a flat `group_tna` object.

## Usage

``` r
as_tna(x)

# S3 method for class 'cluster_summary'
as_tna(x)

# S3 method for class 'mcml'
as_tna(x)

# Default S3 method
as_tna(x)
```

## Arguments

- x:

  A `cluster_summary` object created by
  [`csum`](https://sonsoles.me/cograph/reference/csum.md), or an `mcml`
  object created by
  [`summarize_clusters`](https://sonsoles.me/cograph/reference/summarize_clusters.md).
  A tna object is returned unchanged, and any other input is an error.
  The weights are passed to
  [`tna::tna()`](https://sonsoles.me/tna/reference/build_model.html),
  which row-normalizes them, so a summary computed with `type = "raw"`
  gives the same transition probabilities as one computed with
  `type = "tna"`.

## Value

A `group_tna` object, a flat named list of tna objects. The first
element is named `"macro"` and holds the cluster-level transitions. The
remaining elements are named by cluster and hold the internal
transitions of each cluster.

- macro:

  A tna object of cluster-level transitions, with `weights` (k x k
  transition matrix), `inits` (initial distribution) and `labels`
  (cluster names).

- \<cluster_name\>:

  One tna object per cluster, with `weights` (n_i x n_i matrix), `inits`
  (initial distribution) and `labels` (node labels). A cluster that
  cannot become a tna model is left out with a warning (see Excluded
  Clusters).

A `group_tna` object (flat list of tna objects: macro + per-cluster).

A `group_tna` object (flat list of tna objects: macro + per-cluster).

A `tna` object constructed from the input.

## Details

### Requirements

The tna package must be installed. Without it, the function raises an
error.

### Excluded Clusters

A per-cluster tna cannot be created when:

- The cluster has only 1 node (no internal transitions possible)

- Some nodes in the cluster have no outgoing edges (row sums to 0)

These clusters are left out of the result with a warning of class
`cograph_cluster_dropped`, which names each cluster and the nodes that
have no transition within it. The macro (cluster-level) model still
includes all clusters.

## See also

[`csum`](https://sonsoles.me/cograph/reference/csum.md) to create the
input object,
[`plot_mcml`](https://sonsoles.me/cograph/reference/plot_mcml.md) for
visualization without conversion,
[`tna::tna`](https://sonsoles.me/tna/reference/build_model.html) for the
underlying tna constructor

## Examples

``` r
clusters <- list(C1 = c("Explore", "Reflect", "Discuss"),
                 C2 = c("Plan", "Create", "Share"),
                 C3 = c("Monitor", "Adapt", "Synthesize", "Evaluate"))
as_tna(csum(regulation_net, clusters = clusters, type = "tna"))
#> macro :
#> State Labels : 
#> 
#>    C1, C2, C3 
#> 
#> Transition Probability Matrix :
#> 
#>           C1        C2        C3
#> C1 0.6521739 0.2546584 0.0931677
#> C2 0.1500000 0.2777778 0.5722222
#> C3 0.4036364 0.1745455 0.4218182
#> 
#> Initial Probabilities : 
#> 
#>        C1        C2        C3 
#> 0.3391960 0.2374372 0.4233668 
#> 
#> C1 :
#> State Labels : 
#> 
#>    Explore, Reflect, Discuss 
#> 
#> Transition Probability Matrix :
#> 
#>           Explore   Reflect Discuss
#> Explore 0.0000000 1.0000000       0
#> Reflect 1.0000000 0.0000000       0
#> Discuss 0.4615385 0.5384615       0
#> 
#> Initial Probabilities : 
#> 
#>   Explore   Reflect   Discuss 
#> 0.3333333 0.6666667 0.0000000 
#> 
#> C2 :
#> State Labels : 
#> 
#>    Plan, Create, Share 
#> 
#> Transition Probability Matrix :
#> 
#>        Plan    Create     Share
#> Plan      0 0.3571429 0.6428571
#> Create    0 0.0000000 1.0000000
#> Share     1 0.0000000 0.0000000
#> 
#> Initial Probabilities : 
#> 
#>   Plan Create  Share 
#>   0.21   0.20   0.59 
#> 
#> C3 :
#> State Labels : 
#> 
#>    Monitor, Adapt, Synthesize, Evaluate 
#> 
#> Transition Probability Matrix :
#> 
#>              Monitor     Adapt Synthesize Evaluate
#> Monitor    0.0000000 1.0000000          0        0
#> Adapt      0.0000000 0.0000000          1        0
#> Synthesize 1.0000000 0.0000000          0        0
#> Evaluate   0.4342105 0.5657895          0        0
#> 
#> Initial Probabilities : 
#> 
#>    Monitor      Adapt Synthesize   Evaluate 
#>  0.3448276  0.5086207  0.1465517  0.0000000 
#> 
```
