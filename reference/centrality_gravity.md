# Gravity Centrality

Gravity centrality (Ma et al. 2016) treats node masses as attracting
each other with a force that falls with the squared hop distance, summed
over the nodes within a radius \\r\\: \$\$G(i) = \sum\_{j:\\ 0 \<
d\_{ij} \le r} \frac{m_i m_j}{d\_{ij}^2}.\$\$ The default uses the
k-shell index as mass and \\r = 3\\ (Ma et al. 2016). Degree mass
without truncation is the gravity model of Li et al. (2019, eq. 1), and
degree mass with `gravity_radius = "auto"` is their local gravity model
(eq. 2).

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

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  Direction for directed networks: `"all"` (default), `"out"` or `"in"`.

- gravity_mass:

  Node mass: `"kshell"` (default), `"degree"` or `"legacy"`.

- gravity_radius:

  Largest hop distance included: a number (default 3), `"auto"`, or
  `NULL` for the whole network.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Distances are hop counts, so edge weights are ignored. `mode` sets the
direction of both the distances and the degree or k-shell masses, and
`mode = "all"` treats edges as undirected. The `"auto"` radius is half
the mean finite positive distance, rounded to the nearest integer with a
minimum of 1 (Li et al. 2019, eq. 5). A radius below 1 gives a score of
0 for every node. `gravity_mass = "legacy"` with `gravity_radius = NULL`
computes \\\sum_j k_j s_j / d\_{ij}^2\\, with \\k_j\\ the degree,
\\s_j\\ the k-shell index and no mass on the focal node. This form
differs from the formula of Li et al. (2019).

## References

Ma, L.-L., Ma, C., Zhang, H.-F., & Wang, B.-H. (2016). Identifying
influential spreaders in complex networks based on gravity formula.
Physica A, 451, 205-212.

Li, Z., Ren, T., Ma, X., Liu, S., Zhang, Y., & Zhou, T. (2019).
Identifying influential spreaders by gravity model. Scientific Reports,
9, 8387.

## See also

[`centrality_extended_gravity`](https://sonsoles.me/cograph/reference/centrality_extended_gravity.md),
[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_gravity(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>     148.75     163.75     182.50     163.75     145.00     148.75     105.00 
#>   Evaluate     Create      Share 
#>     148.75     167.50     148.75 
```
