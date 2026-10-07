# Relative-Entropy Integrated Evaluation

The relative-entropy integrated evaluation (Chen, Wang and Luo 2016)
combines several centrality indexes into one score. Each index is turned
into a distribution over the nodes, and the score is the distribution
with the smallest total relative entropy to all of them, which is their
normalized geometric mean: \$\$w_i = \frac{\prod\_{j=1}^{m}
u\_{ji}^{1/m}} {\sum\_{l} \prod\_{j=1}^{m} u\_{jl}^{1/m}}\$\$ A positive
index (larger is more important) maps to \\u_i = C_i / \sum_j C_j\\, and
a negative index (smaller is more important) maps to the normalized
complement \\1 - C_i / \sum_j C_j\\.

## Usage

``` r
centrality_relative_entropy(
  x,
  re_indexes = c("degree", "closeness", "betweenness", "constraint"),
  re_negative = NULL,
  ...
)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- re_indexes:

  Character vector of constituent indexes, without repeats. The default
  `c("degree", "closeness", "betweenness", "constraint")` is the
  source's four-index set.

- re_negative:

  Character vector naming the members of `re_indexes` treated as
  negative. The default `NULL` uses the source's declarations, which
  make `constraint` and `largest_component` negative. `character(0)`
  treats every index as positive.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order,
summing to one. With `normalized = TRUE` the scores are divided by their
maximum and no longer sum to one.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights and loops are ignored. The available
indexes are `"degree"`, `"closeness"` (the reciprocal of the distance
sum over reachable partners), `"betweenness"` (summed over ordered
pairs), `"constraint"` (the source's equation 6, whose outer sum runs
over every other node), `"n_components"` and `"largest_component"` (the
components left after deleting the node). The scores sum to one, and a
node that scores zero on any index scores zero overall. An index that is
zero at every node, such as betweenness on a complete graph, raises a
`cograph_undefined_index` error when the measure is requested by name.
Inside `type = "all"` the same case gives a `cograph_undefined_measure`
warning and an `NA` column. The source's equation 6 differs from Burt's
constraint as computed by
[`igraph::constraint()`](https://r.igraph.org/reference/constraint.html).

## References

Chen, B., Wang, Z. and Luo, C. (2016). Integrated evaluation approach
for node importance of complex networks based on relative entropy.
Journal of Systems Engineering and Electronics, 27(6), 1219-1226.
[doi:10.21629/JSEE.2016.06.10](https://doi.org/10.21629/JSEE.2016.06.10)
.

## See also

[`centrality_bridging`](https://sonsoles.me/cograph/reference/centrality_bridging.md),
[`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_relative_entropy(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.09263748 0.11362201 0.12675208 0.12014870 0.10357082 0.09332242 0.07081870 
#>   Evaluate     Create      Share 
#> 0.08726193 0.10712914 0.08473674 
```
