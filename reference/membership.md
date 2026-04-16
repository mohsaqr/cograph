# Get Community Membership

Extracts a named membership vector from a communities result. Works with
both `cograph_communities` data frames and igraph communities objects.

## Usage

``` r
membership(x)
```

## Arguments

- x:

  A cograph_communities or igraph communities object.

## Value

Named integer vector of community assignments.

## Examples

``` r
g <- igraph::make_graph("Zachary")
comm <- community_louvain(g)
membership(comm)
#>  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 
#>  1  1  1  1  2  2  2  1  3  1  2  1  1  1  3  3  2  1  3  1  3  1  3  4  4  4 
#> 27 28 29 30 31 32 33 34 
#>  3  4  4  3  3  4  3  3 
```
