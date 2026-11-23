# Local Efficiency

Computes the average local efficiency across all nodes (Latora and
Marchiori 2001): the mean over the nodes of the efficiency of the
subgraph induced on each node's neighbors (in- and out-neighbors on a
directed network), the distances measured inside that subgraph; a node
with fewer than two neighbors contributes 0. This is
`networkx::local_efficiency()` and the mean of
`centrality(x, measures = "local_efficiency")`.
[`igraph::average_local_efficiency()`](https://r.igraph.org/reference/global_efficiency.html)
instead measures the neighbors' distances in the network without the
node, which can be shorter (cograph reported that form before 2.7.20).

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

  Numeric vector of edge weights. Default NULL uses the graph's `weight`
  attribute when present. NA ignores weights, so every edge has length
  1, whatever `invert_weights` is.

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
  whose only other argument is `directed`; anything else raises an
  "unused argument" error.

## Value

Numeric scalar: the average local efficiency, or `NA` for a graph with
fewer than two nodes. For unweighted simple graphs it lies in \\\[0,
1\]\\. Weighted graphs can exceed 1 when edge distances are below 1.

## References

Latora, V., & Marchiori, M. (2001). Efficient behavior of small-world
networks. *Physical Review Letters*, 87(19), 198701.
[doi:10.1103/PhysRevLett.87.198701](https://doi.org/10.1103/PhysRevLett.87.198701)

## Examples

``` r
network_local_efficiency(regulation_net)
#> [1] 1.931587
```
