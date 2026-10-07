# Get Number of Communities

Get Number of Communities

## Usage

``` r
n_communities(x)
```

## Arguments

- x:

  A `cograph_communities` object.

## Value

A single integer, the number of distinct communities.

## Examples

``` r
comm <- community_walktrap(regulation_net)
n_communities(comm)
#> [1] 2
```
