# Leading Eigenvector Community Detection

Detects communities using the leading eigenvector of the modularity
matrix. Hierarchical divisive algorithm.

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

  Network input

- weights:

  Edge weights. NULL uses network weights, NA for unweighted.

- steps:

  Maximum number of splits. Default -1 (until modularity decreases).

- start:

  Starting community structure (membership vector).

- options:

  ARPACK options list. Default uses igraph::arpack_defaults().

- callback:

  Optional callback function called after each split.

- extra:

  Extra argument passed to callback.

- env:

  Environment for callback evaluation.

- ...:

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md)

## Value

A `cograph_communities` object

A `cograph_communities` object. See
[`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md).

## References

Newman, M.E.J. (2006). Finding community structure using the
eigenvectors of matrices. *Physical Review E*, 74, 036104.

## Examples

``` r
g <- igraph::make_graph("Zachary")
comm <- community_leading_eigenvector(g)
membership(comm)
#>  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 
#>  1  3  3  3  1  1  1  3  2  2  1  1  3  3  2  2  1  3  2  3  2  3  2  4  4  4 
#> 27 28 29 30 31 32 33 34 
#>  2  4  4  2  2  4  2  2 
net <- as_cograph(matrix(runif(25), 5, 5))
com_le(net)
#> Community structure (leading_eigenvector)
#>   Nodes: 5  | Communities: 2  | Modularity: 0.0413 
#>   Sizes: 2, 3 
#> 
#>  node community
#>     1         1
#>     2         2
#>     3         2
#>     4         2
#>     5         1
```
