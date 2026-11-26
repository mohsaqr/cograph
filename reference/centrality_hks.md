# Hierarchical k-Shell Centrality

Hierarchical k-shell centrality (HKS; Zareie and Sheikhahmadi 2018)
combines the degree of a node with two positions in a peeling of the
network, and sums the result over two steps: \$\$S(i) = \sum\_{j \in
N(i)} k_j (b_j + f_j), \qquad HKS(i) = \sum\_{j \in N(i)} S(j).\$\$
\\b_j\\ is the round in which node \\j\\ is removed when the nodes of
lowest degree are peeled off round by round, and \\f_j = \max_u (b_u -
d\_{ju})\\ measures how close \\j\\ lies to the nodes removed last.

## Usage

``` r
centrality_hks(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The peeling starts at degree 1, removes in each round every remaining
node whose degree does not exceed the current level, and raises the
level only when a round removes nothing. The maximum that defines \\f\\
runs over the nodes that can be reached, so each component is scored on
its own. Isolated nodes score 0. The implementation reproduces the
monotonicity value the source prints for the karate club network
(0.9542); the source prints no node scores on a network whose ties it
lists. `normalized = TRUE` divides the scores by their maximum.

## References

Zareie, A. and Sheikhahmadi, A. (2018). A hierarchical approach for
influential node ranking in complex social networks. Expert Systems with
Applications, 93, 200-211.
[doi:10.1016/j.eswa.2017.10.018](https://doi.org/10.1016/j.eswa.2017.10.018)
.

## See also

[`centrality_iksd`](https://sonsoles.me/cograph/reference/centrality_iksd.md),
[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_hks(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>       1027       1320       1450       1277       1047       1009        839 
#>   Evaluate     Create      Share 
#>       1073       1320       1102 
```
