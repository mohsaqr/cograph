# Long-Range Interaction Centrality (Maximal Path)

The long-range interaction centrality (LRIC; Aleskerov et al. 2017)
measures the influence of a node on the others, directly and through
chains of intermediaries. A group of nodes that send ties to \\j\\ is
critical when their weights sum to at least the quota \\q_j\\, and a
member is pivotal when the group falls below the quota without it. The
direct influence of \\i\\ on \\j\\ is \$\$c\_{ij} = \frac{w\_{ij}}{\min
\sum\_{l \in \Omega(j)} w\_{lj}},\$\$ the minimum taken over the
critical groups \\\Omega(j)\\ in which \\i\\ is pivotal, and 0 when
there is none. The influence along a path is the product of the direct
influences on its ties, the total influence \\c^\*\_{ij}\\ is that of
the best path, and \$\$LRIC(i) = \sum_j c^\*\_{ij} u_j, \qquad u_j =
\frac{\sum_k w\_{jk}}{\sum_l \sum_k w\_{lk}}.\$\$

## Usage

``` r
centrality_lric_max(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `lric_quota` (default 0.5), `lric_path_length`
  (default `Inf`), `weighted` and `enumeration_limit`.

## Value

A named numeric vector with one score per node, in input node order, or
`NA` at every node when the search was cut short.

## Details

Direction and weights are used: \\w\_{ij}\\ is the influence of \\i\\ on
\\j\\, and an undirected tie acts in both directions. Loops are ignored.
`lric_quota` is the share of the weight entering a node that a group
needs in order to be critical, 0.5 by default as in the example of the
source. `lric_path_length` limits the number of ties of a path; by
default (`Inf`) all simple paths are considered. The program of the
authors limits paths to three ties, which changes some values. A node
that sends no tie, or that is pivotal for nobody, scores 0. The scores
follow the working-paper version of the source (arXiv:1610.05892); the
published chapter was not consulted.

Finding the smallest critical group examines sums over subsets of a
node's in-neighbors. With many distinct weights their number doubles
with every in-neighbor. The measure is therefore costly, computed only
when requested by name or through `include`, and it stops past
`enumeration_limit` sums (default one million): every score is then `NA`
with a `cograph_undefined_measure` warning. Parameters outside their
domain raise a `cograph_bad_parameter` error.

## References

Aleskerov, F., Meshcheryakova, N. and Shvydun, S. (2017). Power in
network structures. In Models, Algorithms, and Technologies for Network
Analysis, Springer Proceedings in Mathematics and Statistics, 79-85.
[doi:10.1007/978-3-319-56829-4_7](https://doi.org/10.1007/978-3-319-56829-4_7)
.

## See also

[`centrality_con`](https://sonsoles.me/cograph/reference/centrality_con.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_lric_max(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.44265398 0.56901913 0.45503393 0.38191915 0.11378778 0.32237258 0.07273039 
#>   Evaluate     Create      Share 
#> 0.37815833 0.35995017 0.72519971 
```
