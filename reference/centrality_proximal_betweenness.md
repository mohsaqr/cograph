# Proximal betweenness centrality

Fractions of shortest paths on which a node is the first or last
intermediate vertex, following Brandes (2008), section 3.2, Algorithm 3.
Paths have unit edge lengths. Each reachable ordered source-destination
pair contributes equally, divided among all its shortest paths.

## Usage

``` r
centrality_proximal_betweenness(x, proximal_variant = "source", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- proximal_variant:

  One of `"source"` (default), `"target"`, `"sum"`, or `"union"`.

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  `normalized = TRUE` divides by the maximum score; all-zero results
  remain zero.

## Value

Named numeric vector in input node order.

## Details

The original terminology calls the last intermediate vertex the proximal
source (a proxy interacting directly with the destination), and the
first intermediate vertex the proximal target. The source variant is the
default. Endpoints are excluded, so paths with fewer than two edges
contribute nothing. The sum variant counts both roles; the union variant
counts a vertex only once when a two-edge path places it in both roles.
These are the two combination options in the paper.

Raw scores sum over ordered pairs, including on undirected graphs,
following the displayed definition and Algorithm 3. They are not halved.
Source and target scores agree on undirected graphs; sum is twice either
score, whereas union removes the two-edge overlap. This convention is
distinct from the usual unordered-pair scaling of undirected
betweenness.

Uses the simple unweighted graph, retaining edge direction. Loops are
removed and repeated edges count once after generic input processing.
Weights, mode, inversion and cutoff do not affect this measure. Weighted
shortest paths and edge-distinct multigraph paths are outside this
implementation's verified domain. Unreachable pairs, isolates and
complete graphs contribute zero; empty graphs return no scores.

Native breadth-first searches and dependency accumulation take O(n(n+m))
time after the current O(n squared) dense graph preparation. Path counts
use double precision; a nonfinite count raises an error instead of
returning invalid fractions. Counts above the exact-integer range can be
rounded, so numerical equivalence is tolerance-based.

## References

Brandes, U. (2008). On variants of shortest-path betweenness centrality
and their generic computation. Social Networks, 30, 136-145.
[doi:10.1016/j.socnet.2007.11.001](https://doi.org/10.1016/j.socnet.2007.11.001)
. Section 3.2, Algorithm 3; author preprint dated 12 November 2007,
pages 7-8.

## Examples

``` r
centrality_proximal_betweenness(igraph::make_graph("Zachary"))
#>           1           2           3           4           5           6 
#> 300.1547619  41.8952381  87.5912698   8.5380952   0.6666667  17.6666667 
#>           7           8           9          10          11          12 
#>  17.6666667   0.0000000  13.6079365   0.5476190   0.6666667   0.0000000 
#>          13          14          15          16          17          18 
#>   0.0000000  13.6515873   0.0000000   0.0000000   0.0000000   0.0000000 
#>          19          20          21          22          23          24 
#>   0.0000000   6.4992063   0.0000000   0.0000000   0.0000000  12.6333333 
#>          25          26          27          28          29          30 
#>   2.1666667   2.8611111   0.0000000  14.3920635   1.3809524   2.5428571 
#>          31          32          33          34 
#>   6.1571429  62.6047619 105.0571429 247.0515873 
centrality_proximal_betweenness(igraph::make_ring(5),
                              proximal_variant = "union")
#> 1 2 3 4 5 
#> 2 2 2 2 2 
```
