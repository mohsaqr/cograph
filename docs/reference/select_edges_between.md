# Select Edges Between Node Sets

Select edges connecting two specified node sets.

## Usage

``` r
select_edges_between(
  x,
  set1,
  set2,
  ...,
  .keep_isolates = FALSE,
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

- .keep_isolates:

  Keep nodes with no edges? Default FALSE.

- keep_format:

  Keep input format? Default FALSE.

- directed:

  Auto-detect if NULL.

## Value

A cograph_network with edges between the two node sets.

## See also

[`select_edges`](http://sonsoles.me/cograph/reference/select_edges.md),
[`select_edges_involving`](http://sonsoles.me/cograph/reference/select_edges_involving.md)

## Examples

``` r
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# Edges between {A, B} and {C, D}
select_edges_between(adj, set1 = c("A", "B"), set2 = c("C", "D"))
#> Cograph network: 4 nodes, 3 edges ( undirected )
#> Source: filtered 
#>   Nodes (4): A, B, C, D
#>   Edges: 3 / 6 (density: 50.0%)
#>   Weights: [0.300, 0.800]  |  mean: 0.567
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     B -- C  0.300
#> Layout: none 
```
