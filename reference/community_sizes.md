# Get Community Sizes

Get Community Sizes

## Usage

``` r
community_sizes(x)
```

## Arguments

- x:

  A cograph_communities object

## Value

Integer vector of community sizes

## Examples

``` r
g <- igraph::make_graph("Zachary")
comm <- community_louvain(g)
community_sizes(comm)
#> [1]  7 10 13  4
```
