# Rich Club Coefficient

Computes the rich club coefficient for a degree threshold `k`, the
density of ties among nodes with degree above `k`. It measures the
tendency of high-degree nodes to connect to each other. Degrees are
taken on the undirected simple skeleton of the network.

## Usage

``` r
network_rich_club(x, k = NULL, normalized = FALSE, n_random = 10, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- k:

  Degree threshold. Only nodes with degree greater than `k` are
  included. Default NULL uses the median degree.

- normalized:

  Logical. If TRUE, divide by the mean coefficient of random graphs with
  the same degree sequence. Default FALSE.

- n_random:

  Number of random graphs for normalization. Default 10.

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  whose only other argument is `directed`; anything else raises an
  "unused argument" error.

## Value

Numeric scalar: the rich club coefficient. A normalized value above 1
indicates a rich club effect. `NA` when fewer than two nodes exceed `k`.

## Reproducibility

When `normalized = TRUE` the null graphs are drawn from the caller's RNG
stream; this function takes no `seed` argument and does not save or
restore `.Random.seed`. Call
[`set.seed()`](https://rdrr.io/r/base/Random.html) beforehand for a
reproducible result.
[`rich_club()`](https://sonsoles.me/cograph/reference/rich_club.md)
offers a `seed` argument, confidence intervals, and the full rich club
curve.

## Examples

``` r
network_rich_club(regulation_net, k = 5)
#> [1] 0.6666667
```
