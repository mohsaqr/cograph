# Local Efficiency

Computes the average local efficiency across all nodes, delegating to
[`igraph::average_local_efficiency()`](https://r.igraph.org/reference/global_efficiency.html).
igraph removes the node and measures the distances between its neighbors
*through the rest of the network*, so the value can exceed the one
Latora & Marchiori (2001) define, which restricts those distances to the
subgraph induced on the neighbors.
`centrality(x, measures = "local_efficiency")` reports the
induced-subgraph form, matching networkx, brainGraph and the Brain
Connectivity Toolbox. Both measure fault tolerance and local
integration; the two agree whenever the neighbors have no detour
available.

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

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  whose only other argument is `directed`; anything else raises an
  "unused argument" error.

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

# Per-node values under the Latora definition
centrality(star, measures = "local_efficiency")
#>   node local_efficiency_all
#> 1    1                    0
#> 2    2                    0
#> 3    3                    0
#> 4    4                    0
#> 5    5                    0
```
