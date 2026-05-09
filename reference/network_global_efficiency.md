# Global Efficiency

Computes the global efficiency of a network - the average of the inverse
shortest path lengths between all pairs of nodes. Higher values indicate
better global communication efficiency. Handles disconnected graphs
gracefully (infinite distances contribute 0).

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

  Logical. Consider edge direction? Default TRUE for directed graphs.

- weights:

  Edge weights (NULL for unweighted). Set to NA to ignore existing
  weights.

- invert_weights:

  Logical or NULL. Invert weights so higher weights = shorter paths?
  Default NULL which auto-detects: TRUE for tna objects, FALSE otherwise
  (matching igraph/sna). Set TRUE for strength/frequency weights (qgraph
  style).

- alpha:

  Numeric. Exponent for weight inversion: distance = 1/weight^alpha.
  Default 1.

- ...:

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md)

## Value

Numeric global efficiency. For unweighted simple graphs this is in
\\\[0, 1\]\\; weighted graphs can exceed 1 when edge distances are below
1.

## Examples

``` r
# Complete graph has efficiency 1
k4 <- matrix(1, 4, 4); diag(k4) <- 0
network_global_efficiency(k4)  # 1
#> [1] 1

# Star has lower efficiency
star <- matrix(c(0,1,1,1, 1,0,0,0, 1,0,0,0, 1,0,0,0), 4, 4)
network_global_efficiency(star)  # ~0.83
#> [1] 0.75
```
