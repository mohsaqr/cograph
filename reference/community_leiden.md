# Leiden Community Detection

Leiden algorithm - an improved version of Louvain that guarantees
well-connected communities. Supports CPM and modularity objectives.

## Usage

``` r
community_leiden(
  x,
  weights = NULL,
  resolution = 1,
  objective_function = c("CPM", "modularity"),
  beta = 0.01,
  initial_membership = NULL,
  n_iterations = 2,
  vertex_weights = NULL,
  seed = NULL,
  ...
)

com_ld(
  x,
  weights = NULL,
  resolution = 1,
  objective_function = c("CPM", "modularity"),
  beta = 0.01,
  initial_membership = NULL,
  n_iterations = 2,
  vertex_weights = NULL,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  Network input

- weights:

  Edge weights. NULL uses network weights, NA for unweighted.

- resolution:

  Resolution parameter. Default 1.

- objective_function:

  Optimization objective: "CPM" (Constant Potts Model) or "modularity".
  Default "CPM".

- beta:

  Parameter for randomness in refinement step. Default 0.01.

- initial_membership:

  Initial community assignments (optional).

- n_iterations:

  Number of iterations. Default 2. Use -1 for convergence.

- vertex_weights:

  Vertex weights for CPM objective.

- seed:

  Random seed for reproducibility. Default NULL.

- ...:

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md)

## Value

A `cograph_communities` object

A `cograph_communities` object. See
[`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md).

## References

Traag, V.A., Waltman, L., & van Eck, N.J. (2019). From Louvain to
Leiden: guaranteeing well-connected communities. *Scientific Reports*,
9, 5233.

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::make_graph("Zachary")

  # Standard Leiden
  comm <- community_leiden(g)

  # Higher resolution for more communities
  comm2 <- community_leiden(g, resolution = 1.5)

  # Modularity objective
  comm3 <- community_leiden(g, objective_function = "modularity")
}
```
