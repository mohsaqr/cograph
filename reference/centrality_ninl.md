# Node and Neighbor Layer Information centrality

Zhu and Wang's NINL initializes each node with the sum of original-graph
degrees in its closed radius-r neighborhood. The paper sets r to the
ceiling of the graph's average shortest-path length. Each iteration then
replaces every node's score by the sum of its neighbors' previous
scores: NINL-p = A^p NINL-0. The paper uses p = 3; zero iterations
returns the initial degree volume. Repeated vertices and edges in these
walks count.

## Usage

``` r
centrality_ninl(x, ninl_order = 3, ninl_radius = NULL, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ninl_order:

  Nonnegative integer iteration count, default 3. At most `2^53 - 1`,
  the consecutive-integer precision of doubles.

- ninl_radius:

  `NULL` for the source-defined automatic radius, or a nonnegative
  integer hop radius, or `Inf` for all reachable nodes. Radius zero uses
  the focal node's degree alone.

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  `normalized = TRUE` divides by the maximum score; all-zero scores
  remain zero. Normalization is optional and is not part of the raw
  definition in the original paper.

## Value

Named numeric vector in input node order.

## Details

Uses simple undirected unweighted topology: either arc creates an edge;
loops and parallel edges are removed. Weights, mode, inversion and
cutoff are ignored. This does not claim a directed or weighted NINL
definition.

The mean path length includes all distinct vertex pairs. For
disconnected graphs it is infinite, so the automatic radius includes
every reachable node in each component. This is an explicit cograph
extension of the paper's connected example; unreachable nodes never
enter the degree sum. Isolates score zero and empty graphs return no
scores. A supplied radius is an explicit generalization of the paper's
automatic-radius rule.

Stepwise propagation evaluates the requested finite iteration count,
without assuming convergence to eigenvector centrality. Exact repeated
floating-point states of period one or two allow the remaining
iterations to be skipped while preserving parity. No tolerance-based
convergence cutoff is used. Normalized scores can alternate on bipartite
graphs. Dense distance calculation and propagation take O(n cubed + p n
squared) time and O(n squared) memory; very large orders can be slow if
no exact repeated state occurs. Raw overflow raises an error. With
maximum normalization, global rescaling after every step avoids
overflow; extremely small relative scores can still underflow in double
precision.

## References

Zhu, J. and Wang, L. (2021). Identifying Influential Nodes in Complex
Networks Based on Node Itself and Neighbor Layer Information. Symmetry,
13, 1570. Section 2.1, equations 1-2 and Table 1.
[doi:10.3390/sym13091570](https://doi.org/10.3390/sym13091570) .

## Examples

``` r
centrality_ninl(igraph::make_graph("Zachary"))
#>     1     2     3     4     5     6     7     8     9    10    11    12    13 
#> 88638 61707 71947 46100 17159 18185 18185 35061 45478 19643 17159 10359 17307 
#>    14    15    16    17    18    19    20    21    22    23    24    25    26 
#> 44851 18973 18973  7070 18260 18973 28050 18973 18260 18973 31526 15216 15597 
#>    27    28    29    30    31    32    33    34 
#> 14987 27380 27503 27768 35531 38367 74226 95680 
centrality_ninl(igraph::make_star(5, mode = "undirected"), ninl_order = 2)
#>  1  2  3  4  5 
#> 32 32 32 32 32 
```
