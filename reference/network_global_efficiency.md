# Global Efficiency

Computes the global efficiency of a network, the average of the inverse
shortest path lengths between all ordered pairs of distinct nodes.
Higher values indicate more efficient global communication. Unreachable
pairs contribute 0. A graph with fewer than two nodes returns `NA`.

## Usage

``` r
network_global_efficiency(
  x,
  directed = NULL,
  weights = NULL,
  invert_weights = NULL,
  alpha = 1,
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- directed:

  Logical or NULL. Consider edge direction? Default NULL, which follows
  the directedness of the converted graph.

- weights:

  Numeric vector of edge weights. Default NULL uses the graph's `weight`
  attribute when present. NA ignores weights when `invert_weights` is
  FALSE.

- invert_weights:

  Logical or NULL. If TRUE, weights are converted to distances as
  \\1/w^{\alpha}\\, so stronger ties give shorter paths. If FALSE,
  weights are used as distances. Default NULL uses TRUE for tna objects
  and FALSE otherwise.

- alpha:

  Numeric. Exponent for weight inversion. Default 1.

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  which accepts no arguments besides `directed`.

## Value

Numeric scalar: the global efficiency. For unweighted simple graphs it
lies in \\\[0, 1\]\\. Weighted graphs can exceed 1 when edge distances
are below 1.

## Examples

``` r
network_global_efficiency(regulation_net)
#> [1] 3.304491
```
