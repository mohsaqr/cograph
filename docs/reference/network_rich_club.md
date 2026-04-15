# Rich Club Coefficient

Computes the rich club coefficient for a given degree threshold k.
Measures the tendency of high-degree nodes to connect to each other. A
normalized version compares to random graphs.

## Usage

``` r
network_rich_club(x, k = NULL, normalized = FALSE, n_random = 10, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- k:

  Degree threshold. Only nodes with degree \> k are included. If NULL,
  uses median degree.

- normalized:

  Logical. Normalize by random graph expectation? Default FALSE.

- n_random:

  Number of random graphs for normalization. Default 10.

- ...:

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md)

## Value

Numeric: rich club coefficient (\> 1 indicates rich club effect when
normalized)

## Examples

``` r
# Scale-free networks often show rich-club effect
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::barabasi.game(50, m = 2)
  network_rich_club(g, k = 5)
}
#> Warning: `barabasi.game()` was deprecated in igraph 2.0.0.
#> ℹ Please use `sample_pa()` instead.
#> [1] 0.2666667
```
