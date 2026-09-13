# Gravity centrality

\\G(i) = \sum_j m_i m_j / d\_{ij}^{2}\\, optionally truncated at
`gravity_radius`. The published members of the family differ only in the
mass and the reach:

## Usage

``` r
centrality_gravity(
  x,
  mode = "all",
  gravity_mass = "kshell",
  gravity_radius = 3,
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object.

- mode:

  Direction: `"all"`, `"out"` or `"in"`.

- gravity_mass:

  `"kshell"` (default), `"degree"`, or `"legacy"`.

- gravity_radius:

  Largest distance to include: a number, `"auto"` for half the mean
  distance, or `NULL` for the whole graph. Default 3.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector, one value per node.

## Details

- Gravity centrality (Ma, Ma, Zhang & Wang 2016):

  k-shell mass, radius 3 – the default.

- Gravity model (Li, Ren, Ma, Liu, Zhang & Zhou 2019, eq. 1):

  `gravity_mass = "degree"`, `gravity_radius = NULL`.

- Local gravity model (same paper, eq. 2):

  `gravity_mass = "degree"`, `gravity_radius = "auto"`, which uses their
  empirical half-mean-distance heuristic (eq. 5). cograph rounds to the
  nearest integer (ties to even), with minimum 1, using finite positive
  distances on disconnected graphs. These rounding and
  disconnected-graph rules are cograph conventions.

## Change in 2.4.8

Before 2.4.8 this measure computed \\\sum_j k_j s_j / d\_{ij}^2\\: the
product of degree and k-shell on the partner, no mass at all on the
focal node, and no truncation. That is not the formula of Li et al.
(2019) that its help page cited, and dropping the focal mass changes the
ranking rather than the scale. The default is now Ma et al. (2016).
`gravity_mass = "legacy"` with `gravity_radius = NULL` reproduces the
earlier values exactly.

## References

Ma, L.-L., Ma, C., Zhang, H.-F., & Wang, B.-H. (2016). Identifying
influential spreaders in complex networks based on gravity formula.
Physica A, 451, 205-212.

Li, Z., Ren, T., Ma, X., Liu, S., Zhang, Y., & Zhou, T. (2019).
Identifying influential spreaders by gravity model. Scientific Reports,
9, 8387.

## See also

[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_kreach`](https://sonsoles.me/cograph/reference/centrality_kreach.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
adj <- matrix(0, 6, 6)
adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
adj <- adj + t(adj)
rownames(adj) <- colnames(adj) <- LETTERS[1:6]
centrality_gravity(adj)
#>         A         B         C         D         E         F 
#>  9.888889  9.888889 14.000000 14.000000  9.888889  9.888889 
centrality_gravity(adj, gravity_mass = "degree", gravity_radius = NULL)
#>        A        B        C        D        E        F 
#> 12.38889 12.38889 24.00000 24.00000 12.38889 12.38889 
```
