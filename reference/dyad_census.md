# Dyad Census

Classifies every dyad (unordered pair of nodes) in a directed network
into one of three mutually exclusive states: **mutual** (M, edges in
both directions), **asymmetric** (A, an edge in exactly one direction),
or **null** (N, no edge between the pair). The dyad census is the
dyad-level companion to
[`triad_census`](https://sonsoles.me/cograph/reference/triad_census.md)
and underlies dyad-based reciprocity.

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

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md).

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
`"reciprocity"` attribute.

## Details

For *undirected* networks every present edge is counted as a mutual dyad
and the asymmetric count is always zero, so the census reduces to a
present/absent split. The total number of dyads is \\n(n-1)/2\\
regardless of direction.

## References

Wasserman, S., & Faust, K. (1994). *Social Network Analysis: Methods and
Applications*. Cambridge University Press.

## See also

[`triad_census`](https://sonsoles.me/cograph/reference/triad_census.md),
[`edge_reciprocity`](https://sonsoles.me/cograph/reference/edge_reciprocity.md),
[`network_summary`](https://sonsoles.me/cograph/reference/network_summary.md)

## Examples

``` r
# Directed network with a mix of mutual and asymmetric ties
adj <- matrix(c(
  0, 1, 1, 0,
  1, 0, 0, 1,
  0, 0, 0, 1,
  0, 0, 0, 0
), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- LETTERS[1:4]
cograph::dyad_census(adj)
#> Dyad Census
#> =================================== 
#>        type count proportion
#>      mutual     1  0.1666667
#>  asymmetric     3  0.5000000
#>        null     2  0.3333333
#> 
#>   Dyads: 6   Directed: TRUE 
#>   Reciprocity (2M / (2M + A)): 0.4 
```
