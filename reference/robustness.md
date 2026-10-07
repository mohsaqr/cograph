# Network Robustness Analysis

Performs a targeted attack or random failure analysis on a network and
computes the size of the largest (weakly) connected component after each
vertex or edge removal.

In a targeted attack, vertices are ranked by degree or betweenness
centrality (edges by edge betweenness) and removed from highest to
lowest. In a random failure analysis, vertices or edges are removed in
random order.

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

  Character string; rank by "betweenness", "degree", or remove in
  "random" order. Default: "betweenness". "degree" is not available for
  edge removals and raises an error.

- strategy:

  Character string. "sequential" (default) recomputes the centrality
  after each removal. "static" computes the centrality once on the
  original network and removes elements in that fixed order, as in
  brainGraph. The argument applies to targeted attacks only.

- n_iter:

  Integer; number of random removal sequences averaged when
  `measure = "random"`. Default: 1000.

- mode:

  Degree mode for directed networks: "all", "in", or "out". Default
  "all". Used only when `measure = "degree"`.

- seed:

  Random seed for reproducibility. Default NULL.

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  whose only other argument is `directed`; anything else raises an
  "unused argument" error.

## Value

A data frame (class "cograph_robustness") with one row per removal step,
from zero removed through all removed (`n + 1` rows, where `n` is the
number of vertices or edges), and columns:

- removed_pct:

  Fraction of vertices/edges removed (0 to 1)

- comp_size:

  Size of largest component after removal (averaged over `n_iter` runs
  when `measure = "random"`)

- comp_pct:

  Ratio of component size to original maximum

- measure:

  The `measure` argument: "betweenness", "degree", or "random"

- type:

  A label for the analysis, one of "Targeted vertex attack", "Targeted
  edge attack", "Random vertex removal" or "Random edge removal"

The last row always has `comp_size = 0`. The original number of vertices
or edges (`"n_original"`) and the original largest-component size
(`"orig_max"`) are stored as attributes.

## Details

A betweenness attack removes the vertices or edges that lie on the most
shortest paths, which bridge different regions of the network. A degree
attack removes the most connected vertices first. Random failure removes
vertices or edges in random order and averages the component sizes over
`n_iter` sequences. Betweenness is computed with igraph, which treats
edge weights as distances.

The sequential strategy updates the ranking after every removal, so the
attack follows the vertices that become new bridges or hubs. The static
strategy ranks once on the original network, as in Albert et al. (2000).

Scale-free networks are typically robust to random failures and
vulnerable to targeted attacks. Random networks degrade more uniformly.

## References

Albert, R., Jeong, H., & Barabasi, A.L. (2000). Error and attack
tolerance of complex networks. *Nature*, 406, 378-381.
[doi:10.1038/35019019](https://doi.org/10.1038/35019019)

## See also

[`plot_robustness`](https://sonsoles.me/cograph/reference/plot_robustness.md),
[`robustness_auc`](https://sonsoles.me/cograph/reference/robustness_auc.md)

## Examples

``` r
robustness(regulation_net, measure = "betweenness")
#>    removed_pct comp_size comp_pct     measure                   type
#> 1          0.0        10      1.0 betweenness Targeted vertex attack
#> 2          0.1         9      0.9 betweenness Targeted vertex attack
#> 3          0.2         8      0.8 betweenness Targeted vertex attack
#> 4          0.3         7      0.7 betweenness Targeted vertex attack
#> 5          0.4         6      0.6 betweenness Targeted vertex attack
#> 6          0.5         5      0.5 betweenness Targeted vertex attack
#> 7          0.6         3      0.3 betweenness Targeted vertex attack
#> 8          0.7         1      0.1 betweenness Targeted vertex attack
#> 9          0.8         1      0.1 betweenness Targeted vertex attack
#> 10         0.9         1      0.1 betweenness Targeted vertex attack
#> 11         1.0         0      0.0 betweenness Targeted vertex attack
```
