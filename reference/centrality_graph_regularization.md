# Graph Regularization Centrality

Graph regularization centrality (Dal Col and Petronetto 2023) is the
reciprocal of the diagonal of the inverse regularized Laplacian, where
\\L\\ is the weighted Laplacian of the undirected network and \\\gamma\\
is `grc_gamma`: \$\$GRC_i = \frac{1}{\left\[(I + \gamma
L)^{-1}\right\]\_{ii}}.\$\$ A larger score means that Laplacian
smoothing retains less of a unit impulse placed at the node.

## Usage

``` r
centrality_graph_regularization(x, grc_gamma = 1, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- grc_gamma:

  Regularization strength, a finite nonnegative number. Default 1.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (use edge weights, default `TRUE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the undirected network with loops removed.
For a directed network the weights of opposite arcs are added, and with
`weighted = FALSE` the simple undirected skeleton is used. Edge weights
must be finite and nonnegative, and a zero weight is an absent edge. At
`grc_gamma = 0` every score is one, an isolated node always scores one,
and within a component of \\n\\ nodes the scores lie between one and
\\n\\. A negative or nonfinite `grc_gamma`, or a weight range beyond
double precision, raises an error. The author software (Dal Col 2023)
approximates the same filter with ten Chebyshev terms, so its values can
differ from the exact inverse computed here.

## References

Dal Col, A., & Petronetto, F. (2023). Graph regularization centrality.
Physica A, 628, 129188.
[doi:10.1016/j.physa.2023.129188](https://doi.org/10.1016/j.physa.2023.129188)
.

Dal Col, A. (2023). GRC. Mendeley Data, version 1.
[doi:10.17632/ns63f5dj86.1](https://doi.org/10.17632/ns63f5dj86.1) .

## See also

[`centrality_laplacian`](https://sonsoles.me/cograph/reference/centrality_laplacian.md),
[`centrality_information`](https://sonsoles.me/cograph/reference/centrality_information.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_graph_regularization(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   2.144003   2.475617   2.454605   2.429099   2.112217   2.246126   1.659057 
#>   Evaluate     Create      Share 
#>   2.339699   2.300689   2.499953 
```
