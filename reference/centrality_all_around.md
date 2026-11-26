# All-Around Score

The all-around score (Hou et al. 2012) is the length of the vector
formed by three rescaled measures of a node, its degree \\k\\, its
betweenness \\C_B\\ and its k-shell index \\k_s\\: \$\$d(i) =
\sqrt{\tilde{k}(i)^2 + \tilde{C}\_B(i)^2 + \tilde{k}\_s(i)^2}.\$\$ A
node scores high when it is high on all three.

## Usage

``` r
centrality_all_around(x, ...)
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
The tilde marks a measure divided by its largest value over the nodes.
The source says only that the three measures are brought to a common
range and gives no formula; dividing by the maximum keeps 0 at 0 and is
defined when a measure is the same for every node. A measure that is 0
for every node adds nothing. The score lies between 0 and the square
root of 3. Isolated nodes score 0. The source prints no value of the
measure, so the implementation is checked against the definition only.
`normalized = TRUE` divides the scores by their maximum.

## References

Hou, B., Yao, Y. and Liao, D. (2012). Identifying all-around nodes for
spreading dynamics in complex networks. Physica A: Statistical Mechanics
and its Applications, 391(15), 4012-4017.
[doi:10.1016/j.physa.2012.02.033](https://doi.org/10.1016/j.physa.2012.02.033)
.

## See also

[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_all_around(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   1.314527   1.543896   1.719317   1.653691   1.425415   1.318513   1.171954 
#>   Evaluate     Create      Share 
#>   1.282439   1.463855   1.271866 
```
