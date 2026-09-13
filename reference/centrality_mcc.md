# Maximal clique centrality

For every maximal clique C containing a vertex, add \\(\|C\|-1)!\\. Only
maximal cliques count: a clique contained in a larger clique is
excluded. This is Chin et al.'s MCC, not a count of all cliques.

## Usage

``` r
centrality_mcc(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  With `normalized = TRUE`, positive scores are divided by their
  maximum.

## Value

Named numeric vector in input node order.

## Details

Uses the simple undirected, unweighted skeleton: either direction
creates an edge, parallel edges count once, and self-loops are removed.
Singleton cliques are excluded, so isolates score zero. This is an
explicit cograph convention consistent with the paper's degree reduction
when neighbors have no edges between them. Reading the printed sum
literally with singleton cliques would instead assign isolates \\0! =
1\\.

Maximal clique enumeration has exponential worst-case cost. MCC is held
back from `centrality(type = "all")`; select it explicitly or use
`include = "mcc"`. Scores use double precision; overflow raises an
error, including any clique with more than 171 vertices. Normalization
happens after raw calculation and does not bypass this limit.

## References

Chin, C. H., et al. (2014). cytoHubba: identifying hub objects and
sub-networks from complex interactome. BMC Systems Biology, 8(Suppl 4),
S11.
[doi:10.1186/1752-0509-8-S4-S11](https://doi.org/10.1186/1752-0509-8-S4-S11)
.

## See also

[`centrality_cross_clique`](https://sonsoles.me/cograph/reference/centrality_cross_clique.md),
[`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md).

## Examples

``` r
centrality_mcc(igraph::make_full_graph(5))
#>  1  2  3  4  5 
#> 24 24 24 24 24 
```
