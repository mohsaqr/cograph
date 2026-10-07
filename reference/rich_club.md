# Rich Club Coefficient

Computes the rich club curve across all prominence thresholds. The
unweighted coefficient (Colizza et al. 2006) measures the density of
ties among prominent nodes. The weighted coefficient (Opsahl et al.
2008) measures whether prominent nodes concentrate the strongest ties of
the network among themselves. A directed network is converted to an
undirected one before the computation, with the weights of reciprocal
edges summed, and self-loops are removed.

## Usage

``` r
rich_club(
  x,
  rich = c("k", "s"),
  weighted = TRUE,
  normalized = TRUE,
  n_random = 100,
  directed = NULL,
  seed = NULL,
  digits = NULL,
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object.

- rich:

  Character. Prominence definition: `"k"` (degree, default) or `"s"`
  (strength, the weighted degree).

- weighted:

  Logical. If TRUE (default), compute the weighted rich club
  coefficient. If FALSE, compute the unweighted version (density among
  rich nodes).

- normalized:

  Logical. If TRUE (default), normalize against degree-preserving random
  graphs and report the null distribution. The null graphs are generated
  with
  [`igraph::sample_degseq()`](https://r.igraph.org/reference/sample_degseq.html),
  which fixes the degree sequence. For a weighted rich club the observed
  edge weights are also reshuffled across the null edges, following
  Opsahl et al. (2008). A null graph that `sample_degseq()` cannot draw
  is left out with a warning of class `"cograph_null_draw_failed"` that
  reports how many draws failed.

- n_random:

  Integer. Number of random graphs for normalization. Default 100.

- directed:

  Logical or NULL. Default NULL (auto-detect).

- seed:

  Integer or NULL. Random seed for the null graphs. When a seed is
  given, the caller's random state is restored on exit. Default NULL.

- digits:

  Integer or NULL. Round all numeric columns, including `threshold`.
  Default NULL.

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  which takes no further arguments, so any argument supplied here raises
  an error.

## Value

A data frame with class `"cograph_rich_club"`, one row per prominence
threshold at which at least two nodes are rich, and columns:

- threshold:

  The prominence cut-off. Nodes with prominence strictly greater than
  this value form the club. Thresholds range over the observed
  prominence values excluding the maximum.

- n_rich:

  Number of club members at that threshold.

- phi:

  Observed rich club coefficient.

- phi_norm, phi_rand, ci_lo, ci_hi:

  Present only when `normalized = TRUE`. They hold the observed
  coefficient divided by the null mean, the null mean itself, and the
  2.5\\ quantiles of the null distribution.

The data frame has zero rows for graphs that are too small, complete, or
regular for any threshold to yield a club. The arguments `rich`,
`weighted`, `normalized` and the original input (`"network"`) are stored
as attributes.

## Details

The unweighted coefficient is \\\phi(k) = 2 E\_{\>k} / (N\_{\>k}
(N\_{\>k} - 1))\\, where \\N\_{\>k}\\ is the number of nodes with
prominence above \\k\\ and \\E\_{\>k}\\ the number of edges among them.

The weighted coefficient is \\\phi^w(k) = W\_{\>k} /
\sum\_{l=1}^{E\_{\>k}} w_l^{ranked}\\, where \\W\_{\>k}\\ is the total
weight of the edges among the rich nodes and \\w_l^{ranked}\\ is the
\\l\\-th largest edge weight in the network.

The normalized coefficient is \\\phi\_{norm} = \phi\_{obs} /
\bar{\phi}\_{rand}\\. A value above 1 indicates rich club ordering
beyond what the degree sequence alone explains.

## Printing and plotting

Printing the result shows the prominence definition, the weighting and
normalization settings, the number of thresholds with `phi_norm > 1` and
the first ten rows of the table.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the result
is documented in
[`plot-results`](https://sonsoles.me/cograph/reference/plot-results.md).

## References

Opsahl, T., Colizza, V., Panzarasa, P. & Ramasco, J.J. (2008).
Prominence and control: The weighted rich-club effect. *Physical Review
Letters*, 101, 168702.

Colizza, V., Flammini, A., Serrano, M.A. & Vespignani, A. (2006).
Detecting rich-club ordering in complex networks. *Nature Physics*, 2,
110-115.

## See also

[`rich_club_local`](https://sonsoles.me/cograph/reference/rich_club_local.md),
[`robustness`](https://sonsoles.me/cograph/reference/robustness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md)

## Examples

``` r
rich_club(regulation_net, n_random = 20, seed = 1)
#> Rich Club Analysis
#> ==================
#>   Prominence: degree 
#>   Weighted: TRUE 
#>   Normalized: TRUE 
#>   Thresholds: 2 
#>   Rich club detected at 1 of 2 thresholds (phi_norm > 1)
#> 
#>  threshold n_rich       phi  phi_norm  phi_rand     ci_lo     ci_hi
#>          4      9 0.9485488 1.0615680 0.8935356 0.8346966 0.9445910
#>          5      4 0.4928230 0.8319812 0.5923487 0.3665767 0.8717803
```
