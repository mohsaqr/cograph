# Expected Force centrality

Computes Lawyer's Expected Force after exactly two transmission events
without recovery. For each seed, enumerate ordered sequences of two
infected-to-susceptible edge transmissions. Each sequence produces a
three-node infected cluster with D outgoing edges to susceptible nodes.
Normalize these D values across all sequences and take their Shannon
entropy using natural logarithms (Lawyer 2015, equation 1).

## Usage

``` r
centrality_expected_force(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  `normalized = TRUE` divides by the maximum score; all-zero results
  remain zero.

## Value

Named numeric vector in input node order.

## Details

Different event orders or transmitting parents remain distinct even when
they infect the same three nodes. A seed and two adjacent neighbors of
an undirected triangle form four sequences, not one. Boundary edges are
counted individually even when they reach the same susceptible node.
This is not entropy over distinct infected sets or over boundary-degree
categories, and is not a probability-weighted epidemic simulation.

Uses the simple unweighted graph, retaining direction. In directed
graphs, only outgoing infected-to-susceptible arcs transmit or
contribute boundary degree, following the paper's directed extension.
Loops and duplicate arcs are removed after generic processing. Weights,
mode, inversion and cutoff do not affect the result. The weighted
extension and horizons other than two events are outside this
implementation.

Zero-degree outcomes use the zero-log-zero entropy limit. If no sequence
can perform two transmissions, or every resulting cluster has zero
onward force, cograph returns zero. The latter is an explicit extension
of the paper's undefined all-zero normalization, not author-code parity.
Isolates and components of at most three nodes therefore score zero. A
single positive-force outcome also has entropy zero. Empty input returns
no scores. The measure is local and does not establish epidemic
probability, outbreak size or predictive accuracy on the supplied graph.

Native computation groups three-node clusters by boundary degree while
preserving their event multiplicities. Worst-case time is O(n cubed),
memory O(n squared), including dense graph preparation. Scores remain
independent between components before maximum normalization.

## References

Lawyer, G. (2015). Understanding the influence of all nodes in a
network. Scientific Reports, 5, 8665. Equations 1 and 2 and the directed
extension in the Weighted graphs section.
[doi:10.1038/srep08665](https://doi.org/10.1038/srep08665) .

## See also

[`centrality_modified_expected_force`](https://sonsoles.me/cograph/reference/centrality_modified_expected_force.md)
for degree adjustment.
[`centrality_expected`](https://sonsoles.me/cograph/reference/centrality_expected.md)
computes a different quantity, the sum of neighbor degrees.

## Examples

``` r
centrality_expected_force(igraph::make_graph("Zachary"))
#>        1        2        3        4        5        6        7        8 
#> 5.665412 4.698245 4.947944 4.210475 3.191167 3.376649 3.376649 3.860885 
#>        9       10       11       12       13       14       15       16 
#> 4.279408 3.271121 3.191167 2.699524 3.066568 4.258639 3.344324 3.344324 
#>       17       18       19       20       21       22       23       24 
#> 1.831514 3.191015 3.344324 3.779170 3.344324 3.191015 3.344324 3.953456 
#>       25       26       27       28       29       30       31       32 
#> 2.639873 2.704936 3.023930 3.710869 3.552187 3.744987 3.904355 4.302340 
#>       33       34 
#> 5.154976 5.753993 
```
