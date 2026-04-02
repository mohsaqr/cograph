# Get Number of Communities

Get Number of Communities

## Usage

``` r
n_communities(x)
```

## Arguments

- x:

  A cograph_communities object

## Value

Integer count of communities

## Examples

``` r
g <- igraph::make_graph("Zachary")
comm <- community_louvain(g)
n_communities(comm)
#> [1] 4
```
