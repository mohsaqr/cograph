# Get Modularity Score

Get Modularity Score

## Usage

``` r
modularity.cograph_communities(x, graph = NULL, ...)
```

## Arguments

- x:

  A cograph_communities object

- graph:

  Optional igraph object for recalculation

- ...:

  Additional arguments

## Value

Numeric modularity value

## Examples

``` r
g <- igraph::make_graph("Zachary")
comm <- community_louvain(g)
igraph::modularity(comm)
#> [1] 0.4188034
```
