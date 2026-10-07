# Get Community Membership

Extracts a named membership vector from a `cograph_communities` data
frame or an igraph `communities` object.

## Usage

``` r
membership(x)
```

## Arguments

- x:

  A `cograph_communities` or igraph `communities` object.

## Value

A numeric vector of community numbers named by node. For an igraph
object it is the igraph `membership` vector.

## Examples

``` r
comm <- community_walktrap(regulation_net)
membership(comm)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          1          2          2          1          1          1          1 
#>   Evaluate     Create      Share 
#>          2          2          2 
```
