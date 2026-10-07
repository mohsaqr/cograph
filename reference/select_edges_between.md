# Select Edges Between Node Sets

Select edges connecting two specified node sets.

## Usage

``` r
select_edges_between(
  x,
  set1,
  set2,
  ...,
  keep_isolates = TRUE,
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- set1:

  Character or integer. First node set (names or indices).

- set2:

  Character or integer. Second node set (names or indices).

- ...:

  Additional filter expressions.

- keep_isolates:

  Keep nodes that end up with no edges? Default TRUE.

- keep_format:

  Keep input format? Default FALSE.

- directed:

  Auto-detect if NULL.

## Value

A cograph_network with edges between the two node sets.

## See also

[`select_edges`](https://sonsoles.me/cograph/reference/select_edges.md),
[`select_edges_involving`](https://sonsoles.me/cograph/reference/select_edges_involving.md)

## Examples

``` r
select_edges_between(regulation_net, set1 = c("Plan", "Monitor"),
                     set2 = c("Adapt", "Reflect"), keep_isolates = FALSE)
#> Cograph network: 3 nodes, 2 edges ( directed )
#> Source: matrix 
#>   Nodes (3): Monitor, Adapt, Reflect
#>   Edges: 2 / 6 (density: 33.3%)
#>   Weights: [0.150, 0.160]  |  mean: 0.155
#>   Strongest edges:
#>     Monitor -> Adapt  0.160
#>     Reflect -> Monitor  0.150
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
