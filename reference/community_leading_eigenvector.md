# Leading Eigenvector Community Detection

Detects communities with the leading eigenvector of the modularity
matrix, splitting the network divisively. A directed graph is collapsed
to an undirected graph with summed edge weights.

## Usage

``` r
community_leading_eigenvector(
  x,
  weights = NULL,
  steps = -1,
  start = NULL,
  options = igraph::arpack_defaults(),
  callback = NULL,
  extra = NULL,
  env = parent.frame(),
  ...
)

com_le(
  x,
  weights = NULL,
  steps = -1,
  start = NULL,
  options = igraph::arpack_defaults(),
  callback = NULL,
  extra = NULL,
  env = parent.frame(),
  ...
)
```

## Arguments

- x:

  Network input.

- weights:

  Edge weights. `NULL` uses the network weights and `NA` runs
  unweighted. Negative weights are replaced by their absolute values.

- steps:

  Maximum number of split attempts. Default -1 (no limit).

- start:

  Starting community structure (membership vector).

- options:

  ARPACK options list. Default
  [`igraph::arpack_defaults()`](https://r.igraph.org/reference/arpack.html).

- callback:

  Optional function called after each split.

- extra:

  Extra argument passed to `callback`.

- env:

  Environment in which `callback` is evaluated.

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

Newman, M.E.J. (2006). Finding community structure using the
eigenvectors of matrices. *Physical Review E*, 74, 036104.

## Examples

``` r
community_leading_eigenvector(regulation_net)
#> Community structure (leading_eigenvector)
#>   Nodes: 10  | Communities: 2  | Modularity: 0.1976 
#>   Sizes: 5, 5 
#> 
#>        node community
#>     Explore         1
#>        Plan         2
#>     Monitor         2
#>       Adapt         1
#>     Reflect         1
#>     Discuss         1
#>  Synthesize         1
#>    Evaluate         2
#>      Create         2
#>       Share         2
```
