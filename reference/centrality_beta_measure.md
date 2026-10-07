# Beta Measure

The beta measure, or BG-index (van den Brink and Gilles 2000), gives
node \\i\\ the sum, over its successors \\j\\, of one divided by the
in-degree of \\j\\. Each node with predecessors thus shares one unit of
domination power equally among them: \$\$\beta_i = \sum\_{j : i \to j}
\frac{1}{d^{in}\_j}.\$\$ The negative variant applies the measure to the
reversed network (Boldi and Vigna 2014).

## Usage

``` r
centrality_beta_measure(x, beta_direction = "positive", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- beta_direction:

  `"positive"` (default) credits the sources of arcs. `"negative"`
  credits their targets.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple unweighted network with direction
kept, so weights, loops and parallel edges are ignored. An undirected
edge is a pair of reciprocal arcs, so on an undirected network both
variants equal the sum of the reciprocal degrees of the neighbors. A
node without successors has positive score zero, and a node without
predecessors has negative score zero. The positive scores sum to the
number of nodes with nonzero in-degree, and the negative scores sum to
the number of nodes with nonzero out-degree.

## References

van den Brink, R. and Gilles, R. P. (2000). Measuring domination in
directed networks. Social Networks, 22, 141-157.
[doi:10.1016/S0378-8733(00)00019-8](https://doi.org/10.1016/S0378-8733%2800%2900019-8)
.

Boldi, P. and Vigna, S. (2014). Axioms for centrality. Internet
Mathematics, 10, 222-262.
[doi:10.1080/15427951.2013.865686](https://doi.org/10.1080/15427951.2013.865686)
.

## See also

[`centrality_indegree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality_prestige_domain`](https://sonsoles.me/cograph/reference/centrality_prestige_domain.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_beta_measure(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.5833333  1.8333333  0.6666667  1.7500000  0.4166667  0.8333333  0.9166667 
#>   Evaluate     Create      Share 
#>  0.7500000  1.2500000  1.0000000 
```
