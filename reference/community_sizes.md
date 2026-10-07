# Get Community Sizes

Get Community Sizes

## Usage

``` r
community_sizes(x)
```

## Arguments

- x:

  A `cograph_communities` object.

## Value

An unnamed integer vector of community sizes, ordered by community
number.

## Examples

``` r
comm <- community_walktrap(regulation_net)
community_sizes(comm)
#> [1] 5 5
```
