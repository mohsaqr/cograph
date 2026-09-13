# Coleman-Theil hierarchy index

Measures concentration of Burt's dyadic constraints over a node's
contacts. Let mutual tie strength be z_ij+z_ji, and p_ij its proportion
of all mutual strength incident to i. With organizational weights fixed
at one, define \$\$c\_{ij}=(p\_{ij}+\sum_q p\_{iq}p\_{qj})^2,\quad
r\_{ij}=c\_{ij}/\operatorname{mean}\_{k\in N(i)}c\_{ik}.\$\$ The index
is \\\sum\_{j\in N(i)}r\_{ij}\log(r\_{ij})/(d_i\log(d_i))\\. Contacts
are distinct nodes with positive mutual strength. Investment proportions
use the full supplied graph, including alters' outside ties.

## Usage

``` r
centrality_coleman_theil(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in input node order.

## Details

Follows Burt's STRUCTURE 4.2 manual (pages 181-183): isolates score zero
and nodes with one contact score one. The general formula is undefined
in these two cases; these are the author's explicit conventions. JUNG's
documented implementation instead returns NaN for isolates. Values range
from zero for equal constraints to one for complete concentration. Input
organizational/oligopoly multipliers from STRUCTURE are not implemented;
they are fixed at one, as in the Zoo's formula.

Finite nonnegative weights are supported. Zero-weight ties are absent,
loops are removed, and remaining parallel edges sum after generic
simplification. Directed ties are combined by summing both directions;
`weighted=FALSE` assigns unit weight to each retained edge before
combining them, so reciprocity can affect mutual investment. Generic
mode, shortest-path inversion and cutoff do not affect the result. Empty
input returns no scores. Components are independent before global
normalization.

The default output is already the unit-interval hierarchy index.
`normalized=TRUE` additionally divides by the largest node score; an
all-zero vector remains zero. Dense native arithmetic costs O(n cubed)
time and O(n squared) memory. Global weight scaling precedes mutual
sums. Unrepresentable positive weight or investment ranges raise an
error; tiny squared constraints may underflow and use the zero-log-zero
limit. Relative deviations of local constraints within 16 machine
epsilons are treated as uniform; a series stabilizes the entropy near
uniformity.

## References

Burt, R. S. (1991). STRUCTURE, version 4.2, Reference Manual, Columbia
University, pages 181-183. These pages reproduce the hierarchy
definition attributed to equation 2.9 in Burt (1992), Structural Holes:
The Social Structure of Competition, Harvard University Press.

## Examples

``` r
centrality_coleman_theil(igraph::make_star(5, mode = "undirected"))
#> 1 2 3 4 5 
#> 0 1 1 1 1 
```
