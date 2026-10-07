# Fluid Communities Detection

Fluid communities algorithm, in which a fixed number of communities
expand and compete for nodes. A directed graph is collapsed to an
undirected graph. A disconnected graph raises a warning and only its
largest component is partitioned, so the result has one row per node of
that component. Edge weights are not used.

## Usage

``` r
community_fluid(x, no.of.communities, ...)

com_fl(x, no.of.communities, ...)
```

## Arguments

- x:

  Network input.

- no.of.communities:

  Number of communities to detect. Required; a missing value raises an
  error.

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  whose only other argument is `directed`; anything else raises an
  "unused argument" error.

## Value

A `cograph_communities` data frame with columns `node` and `community`.
See
[`communities`](https://sonsoles.me/cograph/reference/communities.md)
for its attributes.

## References

Pares, F., Gasulla, D.G., Vilalta, A., Moreno, J., Ayguade, E., Labarta,
J., Cortes, U., & Suzumura, T. (2018). Fluid communities: A competitive,
scalable and diverse community detection algorithm. *Studies in
Computational Intelligence*, 689, 229-240.

## Examples

``` r
community_fluid(regulation_net, no.of.communities = 2)
#> Community structure (fluid)
#>   Nodes: 10  | Communities: 2  | Modularity: NA 
#>   Sizes: 5, 5 
#> 
#>        node community
#>     Explore         1
#>        Plan         1
#>     Monitor         2
#>       Adapt         2
#>     Reflect         2
#>     Discuss         1
#>  Synthesize         2
#>    Evaluate         2
#>      Create         1
#>       Share         1
```
