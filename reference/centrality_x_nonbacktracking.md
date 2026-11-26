# X-Nonbacktracking Centrality

X-nonbacktracking centrality (Torres et al. 2021) estimates how much the
largest eigenvalue of the nonbacktracking matrix of a network falls when
a node is removed. For a node \\c\\, \$\$\alpha_c = \Bigl(\sum_i a\_{ci}
v^i\Bigr)^2 - \sum_i a\_{ci} (v^i)^2,\$\$ where \\v^i = \sum_j a\_{ij}
v\_{j \to i}\\ is the nonbacktracking centrality of node \\i\\ in the
network without \\c\\, and \\v\\ is the principal eigenvector of the
nonbacktracking matrix of that network, scaled so that \\v^{\top} P v =
1\\ with \\P\\ the reversal of every directed edge.

## Usage

``` r
centrality_x_nonbacktracking(x, ...)
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

A named numeric vector with one score per node, in input node order,
with `NA` where the score is undefined.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The eigenvector is computed again for every node, on the network without
that node, and its scaling differs from node to node. The score is the
exact quantity of Proposition 3.7 of the source, and differs from the
approximation of its section 4, which uses the eigenvector of the whole
network. A larger score means a larger fall of the eigenvalue. A node of
degree one scores 0 when the score is defined.

The score is `NA` at a node when the network without it has no principal
nonbacktracking eigenvector. This happens when that network is a forest,
when each of its cyclic parts is a single cycle, or when two separate
parts share the largest eigenvalue. A `cograph_undefined_measure`
warning reports how many nodes are affected. The source does not discuss
these cases.

The measure solves one eigenproblem on the directed edges per node, so
it is costly and is computed only when requested by name or through
`include`. `normalized = TRUE` divides the scores by their maximum.

## References

Torres, L., Chan, K. S., Tong, H. and Eliassi-Rad, T. (2021).
Nonbacktracking eigenvalues under node removal: X-centrality and
targeted immunization. SIAM Journal on Mathematics of Data Science,
3(2), 656-675.
[doi:10.1137/20M1352132](https://doi.org/10.1137/20M1352132) .

## See also

[`centrality_x_degree`](https://sonsoles.me/cograph/reference/centrality_x_degree.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_x_nonbacktracking(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   9.608415  14.784395  19.562282  14.193958   9.301579  10.067080   6.624842 
#>   Evaluate     Create      Share 
#>  11.253623  15.118806  11.201170 
```
