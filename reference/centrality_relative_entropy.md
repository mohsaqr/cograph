# Relative-Entropy Integrated Evaluation

Integrates several centrality indexes into one score without asking the
user to weight them. Each index is first turned into a discrete
distribution over the nodes, and the integrated score is the
distribution that has the smallest total relative entropy to all of
them. Chen, Wang and Luo (2016) show that the minimiser has a closed
form, equation (11):
\\w_i=\prod\_{j=1}^{m}u\_{ji}^{1/m}/\sum\_{i}\prod\_{j=1}^{m}u\_{ji}^{1/m}\\,
the normalised geometric mean of the \\m\\ index distributions. The
result sums to one, so it reads as a share of importance rather than a
raw score.

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

  Character vector of constituent indexes, in any order, without
  repeats. Default is the source's four-index distinctiveness set; see
  the Constituent indexes section for the full vocabulary.

- re_negative:

  Character vector naming which of `re_indexes` are negative, that is,
  mapped by equation (9). Default `NULL` uses the source's own
  declarations, which make `constraint` and `largest_component` negative
  and everything else positive. Pass `character(0)` to treat every
  requested index as positive.

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in input node order, summing to one.

## Details

A *positive* index, where a larger value marks the more important node,
becomes a distribution through equation (8), \\C'(i)=C(i)/\sum_j C(j)\\.
A *negative* index, where a smaller value marks the more important node,
becomes one through equation (9), \\C'(i)=(1-C(i)/\sum_j
C(j))/\sum_k(1-C(k)/\sum_j C(j))\\. Both maps are invariant to rescaling
a positive index, so only the shape of an index matters, never its
units.

The geometric mean is unforgiving: a node that scores exactly zero on
any one index scores exactly zero overall. That is the source's own
printed behaviour – its Table 2 gives zero to the three Kite nodes with
zero betweenness – and cograph reproduces it rather than smoothing it
away.

## Constituent indexes

`re_indexes` accepts the six indexes the source both defines and
declares a direction for. Their default directions are the source's own.

- degree:

  Number of neighbours (section 3.2). Positive.

- closeness:

  Equation (3), \\1/\sum_j l\_{ij}\\, the reciprocal of the raw distance
  sum with no \\\|V\|-1\\ factor. Positive.

- betweenness:

  Equation (4), summed over ordered pairs \\j\ne i\ne k\\, so twice the
  usual unnormalised undirected betweenness. Positive.

- constraint:

  Equation (6), the network constraint coefficient, with the outer sum
  over every other node rather than over the neighbours alone. Negative.

- n_components:

  Number of connected components left after deleting the node (section
  4.2). Positive.

- largest_component:

  Size of the largest component left after deleting the node (section
  4.2). Negative.

The default is the four-index "distinctiveness" set of the source's Kite
study. Passing all six reproduces its six-index column, and passing only
`n_components` and `largest_component` its two-index "destructiveness"
column. Equation (2) clustering and equation (5) eigenvector are defined
in the source but never used and never declared positive or negative,
and equation (7) average path length is infinite as soon as deleting a
node disconnects the graph, so none of them is offered.

## Conventions and undefined cases

Equation (6) is not Burt's constraint. Its outer sum runs over all of
\\V\\, so a node two steps away contributes through the indirect term
alone; on the source's Kite this gives node 1 the printed 1.25 where
[`igraph::constraint()`](https://r.igraph.org/reference/constraint.html)
gives 1. The printed outer limit \\j=1\dots\|V\|\\ would also include
\\j=i\\ and raise that node to 1.5, so cograph excludes \\j=i\\: it is
the only reading that reproduces the printed table.

Equation (3) sums distances over all of \\V\\, which is infinite on a
disconnected graph and would leave the index identically zero. cograph
sums over the reachable partners instead. This agrees with equation (3)
exactly on a connected graph, which is the graph class the source works
in, and is a cograph extension outside it. An isolate reaches nobody, so
cograph gives it closeness zero, and an isolate invests nowhere, so
cograph reads its constraint investment row as zeros; both are cograph
conventions.

There is no defensible value when a requested index is zero at every
node – betweenness on a complete graph, degree on an edgeless one –
because equation (8) then divides by zero, and none when equation (9)'s
denominator \\\|V\|-1\\ vanishes on a single node, or when every node is
zero on some index and equation (11) divides by zero. All three raise a
`cograph_undefined_index` error naming the index; none returns zeros.
Naming the measure yourself always raises. When a tier such as
`centrality(x, type = "all")` asked for it instead, that condition
becomes a `cograph_undefined_measure` warning and an `NA` column, so one
undefined measure does not take the whole tier down – this is what
happens on a complete graph, where the betweenness index of the default
set vanishes.

Uses the simple undirected unweighted skeleton, which is the source
domain: either arc creates one edge, parallel edges count once and loops
are removed. Edge weights, `mode`, `cutoff` and `invert_weights` are
ignored. Empty graphs return no scores. The base of the logarithm in
equation (10) cancels out of equation (11), so the closed form and this
implementation are base-free. Raw output already sums to one;
`normalized = TRUE` divides by the maximum, as elsewhere in
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md), so
the largest share becomes one and the vector no longer sums to one.

Numerical verification establishes agreement with the published
equations and the printed Kite tables, not parity with author software,
which the source does not offer, nor any claim about spreading
performance.

## References

Chen, B., Wang, Z. and Luo, C. (2016). Integrated evaluation approach
for node importance of complex networks based on relative entropy.
Journal of Systems Engineering and Electronics, 27(6), 1219-1226.
Equations 3, 4, 6, 8, 9, 10 and 11 and Tables 1-3.
[doi:10.21629/JSEE.2016.06.10](https://doi.org/10.21629/JSEE.2016.06.10)
.

## See also

[`centrality_bridging`](https://sonsoles.me/cograph/reference/centrality_bridging.md)
for the nearest existing cograph measure by rank correlation, and
[`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)
for every measure's orientation.

## Examples

``` r
# The source's own Kite study: four distinctiveness indexes.
centrality_relative_entropy(igraph::make_graph("Krackhardt kite"))
#>          1          2          3          4          5          6          7 
#> 0.09104231 0.09104231 0.00000000 0.15124273 0.00000000 0.18054721 0.18054721 
#>          8          9         10 
#> 0.17762737 0.12795088 0.00000000 

# Only the two destructiveness indexes of its section 4.2.
centrality_relative_entropy(
  igraph::make_graph("Krackhardt kite"),
  re_indexes = c("n_components", "largest_component")
)
#>          1          2          3          4          5          6          7 
#> 0.09211937 0.09211937 0.09211937 0.09211937 0.09211937 0.09211937 0.09211937 
#>          8          9         10 
#> 0.13193611 0.13110891 0.09211937 

# Any subset works, and any index can be re-declared negative.
centrality_relative_entropy(
  igraph::make_tree(7, children = 2, mode = "undirected"),
  re_indexes = c("degree", "closeness"), re_negative = "closeness"
)
#>         1         2         3         4         5         6         7 
#> 0.1556705 0.1926531 0.1926531 0.1147558 0.1147558 0.1147558 0.1147558 
```
