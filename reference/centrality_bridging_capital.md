# Bridging capital from lost information walks

Implements Jackson's section 3.3 definition:
\$\$Brid_i=\sum_j\sum\_{s,t}v\_{st}\sum\_{h=1}^T
\[P^h-(P-P\_{ij}E\_{ij})^h\]\_{st}.\$\$ P contains per-contact
transmission probabilities between zero and one. Rows need not sum to
one: this is broadcast information flow, not a Markov chain.
`bridging_steps` is the finite horizon T, default two, with zero giving
an empty sum. Input edge weights supply P; unweighted edges use
probability one. Finite nonnegative pair values v_st default to one,
including diagonal entries. Named value matrices are reordered by
labels.

## Usage

``` r
centrality_bridging_capital(x, bridging_steps = 2, bridging_values = NULL, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- bridging_steps:

  Nonnegative integer horizon, default two.

- bridging_values:

  Optional nonnegative n by n source-destination information-value
  matrix; NULL uses ones. Both dimensions may be named.

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in input node order.

## Details

The source explicitly deletes one matrix entry P_ij and credits its
criticality to i. On undirected input, opposite entries are therefore
tested separately; deleting one leaves the reverse entry present. This
is not simultaneous deletion of an undirected edge or of a whole node.
Walks can repeat nodes and edges. A walk using the selected entry
several times contributes once to that entry's deletion loss, not once
per use.

Direction and loops are retained, as allowed by the source's formal
definitions. Generic loops/simplify apply first. Remaining parallel
weights sum into one matrix entry and must still be at most one; removal
deletes that aggregate entry. Zero weights are absent. Mode, inversion
and shortest-path cutoff do not affect results. Isolates score zero,
empty inputs return no scores, and all-zero values or zero horizon give
zeros. No renormalization follows entry removal.

The native implementation tracks walks that have and have not used the
selected entry, avoiding cancellation in matrix-power subtraction. Dense
cost is O(m T n cubed) time and O(n squared) memory, where m is the
number of positive directed matrix entries. Request this costly measure
explicitly. Nonrepresentable intermediate walk masses raise errors, even
if a final rescaled result might exist. Raw valued-score overflow may be
avoided by `normalized=TRUE`, which scales values first then divides
final node scores by their maximum. This implements expected walk counts
EInf, not the source's alternative probability-of-ever-hearing measure
PInf.

## References

Jackson, M. O. (2020). A typology of social capital and associated
network measures. Social Choice and Welfare, 54, 311-336.
[doi:10.1007/s00355-019-01189-3](https://doi.org/10.1007/s00355-019-01189-3)
. Definition read in author preprint arXiv:1711.09504v3 (2019), section
3.3, page 18; transmission model section 3.1 and formal graph
conventions section 2.

## Examples

``` r
centrality_bridging_capital(igraph::make_ring(4), bridging_steps = 2)
#>  1  2  3  4 
#> 10 10 10 10 
```
