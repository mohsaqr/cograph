# Dyad Census

Classifies every dyad (unordered pair of nodes) in a directed network
into one of three mutually exclusive states: mutual (M, edges in both
directions), asymmetric (A, an edge in exactly one direction), or null
(N, no edge between the pair). The dyad census is the dyad-level
counterpart of
[`triad_census`](https://sonsoles.me/cograph/reference/triad_census.md)
and is the basis of dyad-based reciprocity.

## Usage

``` r
dyad_census(x, directed = NULL, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object.

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected.

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  which accepts no further arguments. Any argument supplied here raises
  an error.

## Value

A tidy data.frame of class `"cograph_dyad_census"` with one row per dyad
type and columns:

- type:

  Character: `"mutual"`, `"asymmetric"`, or `"null"`.

- count:

  Integer: number of dyads of that type.

- proportion:

  Numeric: count divided by the total number of dyads (\\n(n-1)/2\\).

The dyad-based reciprocity \\2M / (2M + A)\\ is attached as the
`"reciprocity"` attribute (`NA` for a network without edges). The
attributes `"directed"` and `"n_dyads"` record the directedness and the
total number of dyads.

## Details

In an undirected network every present edge is counted as a mutual dyad
and the asymmetric count is zero. The total number of dyads is
\\n(n-1)/2\\ in both cases.

## References

Wasserman, S., & Faust, K. (1994). *Social Network Analysis: Methods and
Applications*. Cambridge University Press.

## See also

[`triad_census`](https://sonsoles.me/cograph/reference/triad_census.md),
[`edge_reciprocity`](https://sonsoles.me/cograph/reference/edge_reciprocity.md),
[`network_summary`](https://sonsoles.me/cograph/reference/network_summary.md)

## Examples

``` r
cograph::dyad_census(regulation_net)
#> Dyad Census
#> =================================== 
#>        type count proportion
#>      mutual     3 0.06666667
#>  asymmetric    24 0.53333333
#>        null    18 0.40000000
#> 
#>   Dyads: 45   Directed: TRUE 
#>   Reciprocity (2M / (2M + A)): 0.2 
```
