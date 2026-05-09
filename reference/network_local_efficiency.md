# Local Efficiency

Computes the average local efficiency across all nodes. Local efficiency
of a node is the global efficiency of its neighborhood subgraph
(excluding the node itself). Measures fault tolerance and local
integration.

## Usage

``` r
network_local_efficiency(
  x,
  weights = NULL,
  invert_weights = NULL,
  alpha = 1,
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- weights:

  Edge weights (NULL for unweighted). Set to NA to ignore existing
  weights.

- invert_weights:

  Logical or NULL. Invert weights so higher weights = shorter paths?
  Default NULL which auto-detects: TRUE for tna objects, FALSE otherwise
  (matching igraph/sna). Set TRUE for strength/frequency weights (qgraph
  style).

- alpha:

  Numeric. Exponent for weight inversion. Default 1.

- ...:

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md)

## Value

Numeric average local efficiency. For unweighted simple graphs this is
in \\\[0, 1\]\\; weighted graphs can exceed 1 when edge distances are
below 1.

## Examples

``` r
# Complete graph: removing any node leaves complete subgraph, so local efficiency = 1
k5 <- matrix(1, 5, 5); diag(k5) <- 0
network_local_efficiency(k5)  # 1
#> [1] 1

# Star: neighbors not connected to each other
star <- matrix(c(0,1,1,1,1, 1,0,0,0,0, 1,0,0,0,0, 1,0,0,0,0, 1,0,0,0,0), 5, 5)
network_local_efficiency(star)  # 0
#> [1] 0
```
