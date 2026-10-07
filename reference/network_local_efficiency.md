# Local Efficiency

Computes the average local efficiency across all nodes with
[`igraph::average_local_efficiency()`](https://r.igraph.org/reference/global_efficiency.html).
For each node, igraph removes the node and measures the distances
between its neighbors through the rest of the network. The value can
therefore exceed the Latora and Marchiori (2001) form, which restricts
those distances to the subgraph induced on the neighbors.
`centrality(x, measures = "local_efficiency")` reports the
induced-subgraph form. The two agree when the neighbors have no path
outside their induced subgraph.

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
  whose only other argument is `directed`; anything else raises an
  "unused argument" error.

## Value

Numeric scalar: the average local efficiency, or `NA` for a graph with
fewer than two nodes. For unweighted simple graphs it lies in \\\[0,
1\]\\. Weighted graphs can exceed 1 when edge distances are below 1.

## Examples

``` r
network_local_efficiency(regulation_net)
#> [1] 2.918765
```
