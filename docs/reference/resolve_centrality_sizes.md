# Resolve Centrality-Based Node Sizes

Calculates node sizes based on centrality measures.

## Usage

``` r
resolve_centrality_sizes(
  x,
  scale_by,
  size_range = c(2, 8),
  n = NULL,
  scaling = "default",
  scale_exp = 1
)
```

## Arguments

- x:

  Network object (igraph, matrix, cograph_network, etc.)

- scale_by:

  Centrality measure name or list with measure and parameters. Valid
  measures: "degree", "strength", "betweenness", "closeness",
  "eigenvector", "pagerank", "authority", "hub", "eccentricity",
  "coreness", "constraint", "harmonic". Also accepts directional
  shorthands: "indegree", "outdegree", "instrength", "outstrength",
  "incloseness", "outcloseness", "inharmonic", "outharmonic",
  "ineccentricity", "outeccentricity".

- size_range:

  Numeric vector of length 2: c(min_size, max_size). Default c(2, 8).

- n:

  Number of nodes (for validation).

- scaling:

  Scaling mode: "default" or "legacy".

- scale_exp:

  Dampening exponent applied to normalized centrality values before
  mapping to size range. Default 1 (linear).

## Value

Named list with 'sizes' (vector of node sizes) and 'values' (raw
centrality values).
