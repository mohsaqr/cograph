# Network Robustness Analysis

Performs a targeted attack or random failure analysis on a network,
calculating the size of the largest connected component after sequential
vertex or edge removal.

In a targeted attack, vertices are sorted by degree or betweenness
centrality (or edges by betweenness), and successively removed from
highest to lowest. In a random failure analysis, vertices/edges are
removed in random order.

## Usage

``` r
robustness(
  x,
  type = c("vertex", "edge"),
  measure = c("betweenness", "degree", "random"),
  strategy = c("sequential", "static"),
  n_iter = 1000,
  mode = "all",
  seed = NULL,
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- type:

  Character string; either "vertex" or "edge" removals. Default:
  "vertex"

- measure:

  Character string; sort by "betweenness", "degree", or "random".
  Default: "betweenness"

- strategy:

  Character string; "sequential" (default) recalculates centrality after
  each removal. "static" computes centrality once on the original
  network and removes nodes in that fixed order (brainGraph-style). Only
  affects targeted attacks; random removal is unaffected.

- n_iter:

  Integer; number of iterations for random analysis. Default: 1000
  (matching brainGraph convention)

- mode:

  For directed networks: "all", "in", or "out". Default "all".

- seed:

  Random seed for reproducibility. Default NULL.

- ...:

  Additional arguments passed to
  [`to_igraph`](http://sonsoles.me/cograph/reference/to_igraph.md)

## Value

A data frame (class "cograph_robustness") with columns:

- removed_pct:

  Fraction of vertices/edges removed (0 to 1)

- comp_size:

  Size of largest component after removal

- comp_pct:

  Ratio of component size to original maximum

- measure:

  Attack strategy used

- type:

  Type of analysis (vertex or edge removal)

## Details

Three attack strategies are available:

**Targeted Attack - Betweenness (default):** Vertices/edges are sorted
by betweenness centrality and removed from highest to lowest. This
targets nodes that bridge different network regions.

**Targeted Attack - Degree:** Vertices are sorted by degree and removed
from highest to lowest. This targets highly connected hub nodes. Note:
for edge attacks, degree is not available; use betweenness instead.

**Random Failure:** Vertices/edges are removed in random order, averaged
over n_iter iterations. This simulates random component failures.

**Strategy:** The `strategy` parameter controls how targeted attacks
work:

- `"sequential"` (default): Recalculates centrality after each removal.
  This is a stronger attack because removing a hub changes which nodes
  become the new bridges/hubs.

- `"static"`: Computes centrality once on the original network and
  removes nodes in that fixed order (as in brainGraph). This matches the
  original Albert et al. (2000) method.

Scale-free networks are typically robust to random failures but
vulnerable to targeted attacks, while random networks degrade more
uniformly.

## References

Albert, R., Jeong, H., & Barabasi, A.L. (2000). Error and attack
tolerance of complex networks. *Nature*, 406, 378-381.
[doi:10.1038/35019019](https://doi.org/10.1038/35019019)

## See also

[`plot_robustness`](http://sonsoles.me/cograph/reference/plot_robustness.md),
[`robustness_auc`](http://sonsoles.me/cograph/reference/robustness_auc.md)

## Examples

``` r
# Create a scale-free network
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::sample_pa(50, m = 2, directed = FALSE)

  # Targeted attack by betweenness
  rob_btw <- robustness(g, measure = "betweenness")

  # Targeted attack by degree
  rob_deg <- robustness(g, measure = "degree")

  # Random failure
  rob_rnd <- robustness(g, measure = "random", n_iter = 50)

  # View results
  head(rob_btw)
}
#>   removed_pct comp_size comp_pct     measure                   type
#> 1        0.00        50     1.00 betweenness Targeted vertex attack
#> 2        0.02        49     0.98 betweenness Targeted vertex attack
#> 3        0.04        46     0.92 betweenness Targeted vertex attack
#> 4        0.06        41     0.82 betweenness Targeted vertex attack
#> 5        0.08        40     0.80 betweenness Targeted vertex attack
#> 6        0.10        37     0.74 betweenness Targeted vertex attack
```
