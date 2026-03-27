# Infomap Community Detection

Information-theoretic community detection based on random walk dynamics.
Minimizes the map equation (description length of random walks).

## Usage

``` r
community_infomap(
  x,
  weights = NULL,
  v.weights = NULL,
  nb.trials = 10,
  modularity = TRUE,
  seed = NULL,
  ...
)

com_im(
  x,
  weights = NULL,
  v.weights = NULL,
  nb.trials = 10,
  modularity = TRUE,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  Network input

- weights:

  Edge weights for transitions. NULL uses network weights.

- v.weights:

  Vertex weights (teleportation weights).

- nb.trials:

  Number of optimization trials. Default 10.

- modularity:

  Logical; calculate modularity? Default TRUE.

- seed:

  Random seed for reproducibility. Default NULL.

- ...:

  Additional arguments passed to
  [`to_igraph`](http://sonsoles.me/cograph/reference/to_igraph.md)

## Value

A `cograph_communities` object

A `cograph_communities` object. See
[`detect_communities`](http://sonsoles.me/cograph/reference/detect_communities.md).

## References

Rosvall, M., & Bergstrom, C.T. (2008). Maps of random walks on complex
networks reveal community structure. *PNAS*, 105(4), 1118-1123.

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::make_graph("Zachary")
  comm <- community_infomap(g, nb.trials = 20)
}
```
