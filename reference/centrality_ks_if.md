# K-Shell Iteration Factor

The k-shell iteration factor (KS-IF; Wang et al. 2016) refines the
k-shell index by the iteration in which a node is removed within its
shell, and sums the result, weighted by degree, over a node and its
neighbors: \$\$IC(i) = \delta_i k_i + \sum\_{j \in N(i)} \delta_j k_j,
\qquad \delta_i = ks_i \Bigl(1 + \frac{n_i}{m_i}\Bigr),\$\$ where
\\ks_i\\ is the k-shell index of \\i\\, \\m_i\\ the number of iterations
the decomposition needs to remove that shell and \\n_i\\ the iteration
in which \\i\\ is removed.

## Usage

``` r
centrality_ks_if(x, ...)
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
Within the stage of shell \\k\\, an iteration removes every remaining
node whose degree is at most \\k\\; iterations are counted from 1 in
each shell and over the whole network, so components share them. The
nodes removed last in a shell have \\\delta = 2\\ks\\. Isolated nodes
score 0. No published value checks the implementation: the source's
illustration cannot be produced by this decomposition on any network,
and its two tables disagree about one node. `normalized = TRUE` divides
the scores by their maximum.

## References

Wang, Z., Zhao, Y., Xi, J. and Du, C. (2016). Fast ranking influential
nodes in complex networks using a k-shell iteration factor. Physica A:
Statistical Mechanics and its Applications, 461, 171-181.
[doi:10.1016/j.physa.2016.05.048](https://doi.org/10.1016/j.physa.2016.05.048)
.

## See also

[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_hks`](https://sonsoles.me/cograph/reference/centrality_hks.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_ks_if(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        236        282        325        269        211        244        202 
#>   Evaluate     Create      Share 
#>        265        297        275 
```
