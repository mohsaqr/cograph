# Node Importance Evaluation Matrix Centrality

The node importance evaluation matrix (NIEM) method (Zhou et al. 2012)
multiplies the efficiency of a node by the importance its neighbors
contribute: \$\$C_i = I_i \sum\_{j \in N(i)} \frac{D_j I_j}{\langle k
\rangle^2}, \qquad I_k = \frac{1}{n} \sum\_{i \ne k}
\frac{1}{d\_{ki}},\$\$ with \\D\\ the degree, \\\langle k \rangle\\ the
mean degree and \\d\\ the shortest-path distance.

## Usage

``` r
centrality_niem(x, ...)
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
network, so direction, weights, loops and parallel edges are ignored. A
pair of nodes without a path adds nothing to the efficiency, as the
source defines. The mean degree and \\n\\ are those of the whole
network. Isolated nodes score 0, and so does every node of a network
without edges. `normalized = TRUE` divides the scores by their maximum.

## References

Zhou, X., Zhang, F.-M., Li, K.-W., Hui, X.-B. and Wu, H.-S. (2012).
Finding vital node by node importance evaluation matrix in complex
networks. Acta Physica Sinica, 61(5), 050201.
[doi:10.7498/aps.61.050201](https://doi.org/10.7498/aps.61.050201) .

## See also

[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_niem(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.4681070  0.5967078  0.7297668  0.5709877  0.4489026  0.4921125  0.4034636 
#>   Evaluate     Create      Share 
#>  0.5425240  0.6198560  0.5425240 
```
