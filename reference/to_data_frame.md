# Export Network as Edge List Data Frame

Converts a network to an edge list data frame with columns for source,
target, and weight.

## Usage

``` r
to_data_frame(x, directed = NULL)

to_df(x, directed = NULL)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object.

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected.

## Value

A data frame with columns:

- `from`: Source node name/label

- `to`: Target node name/label

- `weight`: Edge weight

## See also

`to_df`,
[`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
[`as_cograph`](https://sonsoles.me/cograph/reference/as_cograph.md)

## Examples

``` r
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# Convert to edge list
to_data_frame(adj)
#>   from to weight
#> 1    A  B    0.5
#> 2    A  C    0.8
#> 3    B  C    0.3
#> 4    B  D    0.6
#> 5    C  D    0.4

# Use alias
to_df(adj)
#>   from to weight
#> 1    A  B    0.5
#> 2    A  C    0.8
#> 3    B  C    0.3
#> 4    B  D    0.6
#> 5    C  D    0.4
```
