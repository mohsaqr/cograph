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

A base `data.frame` with one row per edge and exactly three columns:

- `from`: Source node name/label

- `to`: Target node name/label

- `weight`: Edge weight

Further edge columns, such as `session` or `time` from temporal edge
lists, are dropped.
[`get_edges`](https://sonsoles.me/cograph/reference/get_nodes.md)
returns the full edge table. An undirected network contributes one row
per unordered pair.

## See also

`to_df`,
[`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
[`as_cograph`](https://sonsoles.me/cograph/reference/as_cograph.md)

## Examples

``` r
to_data_frame(regulation_net)
#>          from         to weight
#> 1       Adapt    Explore   0.28
#> 2     Reflect    Explore   0.05
#> 3     Discuss    Explore   0.30
#> 4      Create    Explore   0.14
#> 5  Synthesize       Plan   0.11
#> 6       Share       Plan   0.21
#> 7        Plan    Monitor   0.13
#> 8     Reflect    Monitor   0.15
#> 9  Synthesize    Monitor   0.07
#> 10   Evaluate    Monitor   0.33
#> 11     Create    Monitor   0.17
#> 12      Share    Monitor   0.49
#> 13    Monitor      Adapt   0.16
#> 14   Evaluate      Adapt   0.43
#> 15      Share      Adapt   0.39
#> 16    Explore    Reflect   0.35
#> 17    Discuss    Reflect   0.35
#> 18 Synthesize    Reflect   0.42
#> 19   Evaluate    Reflect   0.07
#> 20       Plan    Discuss   0.40
#> 21      Adapt    Discuss   0.34
#> 22      Adapt Synthesize   0.17
#> 23       Plan   Evaluate   0.49
#> 24     Create   Evaluate   0.39
#> 25       Plan     Create   0.20
#> 26    Monitor     Create   0.37
#> 27    Discuss     Create   0.14
#> 28    Explore      Share   0.27
#> 29       Plan      Share   0.36
#> 30     Create      Share   0.23
```
