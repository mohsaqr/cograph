# KDEC Centrality

The KDEC method (Zhang et al. 2019) gives every node the weight \\W(v) =
k_s(v)\\d(v)\\, its core number times its degree, and sums the influence
a node exchanges with the nodes one and two steps away, discounted by an
effective distance: \$\$KDEC(i) = \frac{W(i)}{n}\Bigl\[\sum\_{j \in
N(i)} \frac{W(j)}{e\_{ij}^2} + \sum\_{j \in N(i)} \sum\_{k \in N(j),\\ k
\ne i,\\ k \notin N(i)} \frac{W(k)}{(e\_{ij} + e\_{jk})^2}\Bigr\],\$\$
with \\e\_{ij} = 1 - \log\_{10}(1/d_j)\\ and \\n\\ the number of nodes.

## Usage

``` r
centrality_kdec(x, ...)
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
node two steps away is counted once for every neighbor that leads to it.
That rule and the division by \\n\\ follow the worked example of the
source. Because \\n\\ counts all nodes, adding a component lowers every
score. Isolated nodes score 0. `normalized = TRUE` divides the scores by
their maximum.

## References

Zhang, J., Wang, B., Sheng, J., Dai, J., Hu, J. and Chen, L. (2019).
Identifying influential nodes in complex networks based on local
effective distance. Information, 10(10), 311.
[doi:10.3390/info10100311](https://doi.org/10.3390/info10100311) .

## See also

[`centrality_ked`](https://sonsoles.me/cograph/reference/centrality_ked.md),
[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_kdec(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  116.93746  155.35235  177.12715  169.08084  128.27090  125.83149   86.43374 
#>   Evaluate     Create      Share 
#>  123.66203  150.80295  117.29316 
```
