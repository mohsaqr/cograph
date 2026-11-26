# Diversity-Strength Centrality

Diversity-strength centrality (DSC; Zareie et al. 2019) is the product
of three properties of the neighbors of a node: how deep they lie in the
network on average, how evenly that depth is spread among them, and how
much their own neighborhoods differ: \$\$DSC(v) = \frac{S(v)}{k_v} \\
H_1(v) \\ JSD(v), \qquad S(v) = \sum\_{u \in N(v)} IKs(u).\$\$ \\IKs\\
is the improved k-shell index, \\H_1(v)\\ the entropy of the shares
\\IKs(u)/S(v)\\ and \\JSD(v)\\ the Jensen-Shannon divergence of the
neighbors' shell distributions, where the shell distribution of \\u\\
gives the share of the neighbors of \\u\\ in each shell.

## Usage

``` r
centrality_dsc(x, ...)
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
\\IKs\\ is the index
[`centrality_iksd`](https://sonsoles.me/cograph/reference/centrality_iksd.md)
returns. The shells of the divergence term are those of the improved
index, as section 3.2 of the source states, although the symbols of its
definition name the classic k-shell. The natural logarithm is used; the
source prints no base, and another base multiplies every score by one
constant. A node with one neighbor scores 0, and so does a node whose
neighbors all have the same shell distribution. Isolated nodes score 0.
The source prints no node scores; the implementation reproduces the
monotonicity values it prints for the rankings built on this score (see
[`centrality_dsr`](https://sonsoles.me/cograph/reference/centrality_dsr.md)).
`normalized = TRUE` divides the scores by their maximum.

## References

Zareie, A., Sheikhahmadi, A. and Jalili, M. (2019). Influential node
ranking in social networks based on neighborhood diversity. Future
Generation Computer Systems, 94, 120-129.
[doi:10.1016/j.future.2018.11.023](https://doi.org/10.1016/j.future.2018.11.023)
.

## See also

[`centrality_dsr`](https://sonsoles.me/cograph/reference/centrality_dsr.md),
[`centrality_edsr`](https://sonsoles.me/cograph/reference/centrality_edsr.md),
[`centrality_iksd`](https://sonsoles.me/cograph/reference/centrality_iksd.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_dsc(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  1.5840964  1.4279812  2.4262621  0.7725596  0.5203141  1.3470957  0.8604218 
#>   Evaluate     Create      Share 
#>  1.1666690  1.3056283  1.5110591 
```
