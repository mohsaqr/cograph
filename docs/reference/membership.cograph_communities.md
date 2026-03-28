# Get Community Membership

Get Community Membership

## Usage

``` r
membership.cograph_communities(x)
```

## Arguments

- x:

  A cograph_communities object

## Value

Named integer vector of community assignments

## Examples

``` r
g <- igraph::make_graph("Zachary")
comm <- community_louvain(g)
igraph::membership(comm)
#>  [1] 1 1 1 1 2 2 2 1 3 1 2 1 1 1 3 3 2 1 3 1 3 1 3 4 4 4 3 4 4 3 3 4 3 3
```
