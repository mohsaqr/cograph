# BG-index or beta power measure

The positive beta-measure of node i is the sum, over its successors j,
of one divided by the in-degree of j. Each node with predecessors shares
one unit of domination power equally among those predecessors. This is
van den Brink and Gilles' BG-measure (1992, definition 2.1),
subsequently called the beta-measure (2000, definition 2.1). The
negative variant applies the positive measure to the reversed graph
(Boldi and Vigna 2014). It sums reciprocal source out-degrees over
incoming neighbors.

## Usage

``` r
centrality_beta_measure(x, beta_direction = "positive", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- beta_direction:

  Either `"positive"` (default, credits sources) or `"negative"`
  (credits destinations).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  `normalized = TRUE` divides scores by their maximum; all-zero scores
  remain zero. This differs from normalizing to unit total mass.

## Value

Named numeric vector in input node order.

## Details

Uses the simple unweighted graph, retaining direction. Loops and
duplicate arcs are removed; weights, mode, inversion and cutoff are
ignored. Undirected edges represent reciprocal arcs, so both variants
coincide with the sum of reciprocal neighbor degrees. This does not
implement the separately defined weighted extension of the original
paper.

Nodes without successors have positive score zero; nodes without
predecessors have negative score zero. Isolates score zero, and empty
graphs return no scores. There is no division by a zero degree: every
contributing successor has at least one predecessor. Raw positive scores
sum to the number of nodes with nonzero in-degree; raw negative scores
sum to the number with nonzero out-degree. In disconnected graphs this
accounting applies independently to each component.

Dense matrix preparation and evaluation take O(n squared) time and
memory. The score is an expected number of predecessor selections, not a
probability distribution or a stationary random-walk centrality.

## References

van den Brink, R. and Gilles, R. P. (1992). Measuring domination in
directed graphs. Tilburg Research Memorandum FEW 565, definition 2.1 and
example 2.2, pp. 3-4. van den Brink, R. and Gilles, R. P. (2000).
Measuring domination in directed networks. Social Networks, 22, 141-157,
definition 2.1.
[doi:10.1016/S0378-8733(00)00019-8](https://doi.org/10.1016/S0378-8733%2800%2900019-8)
. Boldi, P. and Vigna, S. (2014). Axioms for centrality. Internet
Mathematics, 10, 222-262.
[doi:10.1080/15427951.2013.865686](https://doi.org/10.1080/15427951.2013.865686)
.

## Examples

``` r
centrality_beta_measure(igraph::make_graph("Zachary"))
#>         1         2         3         4         5         6         7         8 
#> 5.1944444 2.3625000 2.1569444 1.2236111 0.6458333 1.1458333 1.1458333 0.4402778 
#>         9        10        11        12        13        14        15        16 
#> 0.5546569 0.1588235 0.6458333 0.0625000 0.2291667 0.4991013 0.1421569 0.1421569 
#>        17        18        19        20        21        22        23        24 
#> 0.5000000 0.1736111 0.1421569 0.2324346 0.1421569 0.1736111 0.1421569 0.9754902 
#>        25        26        27        28        29        30        31        32 
#> 0.7500000 0.7000000 0.3088235 0.6921569 0.3254902 0.8421569 0.4532680 1.2046569 
#>        33        34 
#> 3.7254902 5.7666667 
centrality_beta_measure(igraph::make_star(5, mode = "out"),
                        beta_direction = "negative")
#>    1    2    3    4    5 
#> 0.00 0.25 0.25 0.25 0.25 
```
