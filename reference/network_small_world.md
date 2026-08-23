# Small-World Coefficient (Sigma)

Computes the small-world coefficient sigma, defined as: sigma = (C /
C_rand) / (L / L_rand) where C is clustering coefficient, L is mean path
length, and \_rand are values from equivalent random graphs.

## Usage

``` r
network_small_world(x, n_random = 10, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- n_random:

  Number of random graphs for comparison. Default 10.

- ...:

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md)

## Value

Numeric: small-world coefficient sigma

## Details

Values \> 1 indicate small-world properties. Typically small-world
networks have sigma \>\> 1.

## Examples

``` r
# Watts-Strogatz small-world graph
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::sample_smallworld(1, 20, 3, 0.1)
  network_small_world(g)  # Should be > 1
}
#> [1] 1.264929
```
