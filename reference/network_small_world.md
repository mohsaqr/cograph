# Small-World Coefficient (Sigma)

Computes the small-world coefficient \$\$\sigma = \frac{C / C\_{rand}}{L
/ L\_{rand}}\$\$ where \\C\\ is the global clustering coefficient, \\L\\
is the mean shortest path length, and \\C\_{rand}\\ and \\L\_{rand}\\
are their means over Erdos-Renyi graphs with the same numbers of nodes
and edges. A directed network is collapsed to undirected first.

## Usage

``` r
network_small_world(x, n_random = 10, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- n_random:

  Number of Erdos-Renyi comparison graphs (same `n` and `m` as the
  observed graph). Default 10.

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  whose only other argument is `directed`; anything else raises an
  "unused argument" error.

## Value

Numeric: small-world coefficient sigma. `NA` when the graph has fewer
than 4 nodes, no edges, or an undefined/zero mean path length.

## Details

Values above 1 indicate small-world structure.

## Reproducibility

The comparison graphs are drawn from the caller's RNG stream; this
function takes no `seed` argument and does not save or restore
`.Random.seed`. Call [`set.seed()`](https://rdrr.io/r/base/Random.html)
beforehand for a reproducible result. A larger `n_random` gives a more
stable estimate.

## Examples

``` r
set.seed(1)
network_small_world(regulation_net, n_random = 5)
#> [1] 3.339275
```
