# Filter Edges by Metadata

Filter edges using dplyr-style expressions on any edge column. Returns a
cograph_network object by default (universal format), or optionally the
same format as input when `keep_format = TRUE`.

## Usage

``` r
filter_edges(
  x,
  ...,
  .keep_isolates = FALSE,
  keep_format = FALSE,
  directed = NULL
)

subset_edges(
  x,
  ...,
  .keep_isolates = FALSE,
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input: cograph_network, matrix, igraph, network, or tna
  object.

- ...:

  Filter expressions using any edge column (e.g., `weight > 0.5`,
  `weight > mean(weight)`, `abs(weight) > 0.3`).

- .keep_isolates:

  Logical. Keep nodes with no remaining edges? Default FALSE.

- keep_format:

  Logical. If TRUE, return the same format as input (matrix returns
  matrix, igraph returns igraph, etc.). Default FALSE returns
  cograph_network (universal format).

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected. Only used for
  non-cograph_network inputs.

## Value

A cograph_network object with filtered edges. If `keep_format = TRUE`,
returns the same type as input (matrix, igraph, network, etc.).

See `filter_edges`.

## See also

[`filter_nodes`](https://sonsoles.me/cograph/reference/filter_nodes.md),
[`splot`](https://sonsoles.me/cograph/reference/splot.md),
`subset_edges`

## Examples

``` r
adj <- matrix(c(0, .5, .8, 0, .5, 0, .3, .6,
                .8, .3, 0, .4, 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# Keep only strong edges
filter_edges(adj, weight > 0.5)
#> Cograph network: 4 nodes, 2 edges ( undirected )
#> Source: filtered 
#>   Nodes (4): A, B, C, D
#>   Edges: 2 / 6 (density: 33.3%)
#>   Weights: [0.600, 0.800]  |  mean: 0.700
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#> Layout: none 

# Matrix in, matrix out
filter_edges(adj, weight > 0.5, keep_format = TRUE)
#>   A B   C   D
#> A 0 0 0.8 0.0
#> B 0 0 0.0 0.6
#> C 0 0 0.0 0.0
#> D 0 0 0.0 0.0

# Pipe-friendly with cograph_network
as_cograph(adj) |>
  filter_edges(weight > 0.3) |>
  filter_nodes(degree >= 2) |>
  splot()
```
