# DK-Based Gravity Model

The DK-based gravity model (Li and Huang 2021) sums, over the partners
within a hop-distance radius \\R\\, the product of the two nodes' masses
divided by their squared distance. The mass \\DK(i) = k(i) + k_s^\*(i)\\
adds the degree to an improved k-shell index \\k_s^\*(i) = k_s(i) +
p(i)/(\max_k q(k) + 1)\\, where \\p(i)\\ is the peeling stage at which
the node leaves its shell and \\q(k)\\ the number of stages shell \\k\\
needs. \$\$DKGM_i = \sum\_{j \ne i,\\ d(i,j) \le R}
\frac{DK(i)\\DK(j)}{d(i,j)^2}\$\$

## Usage

``` r
centrality_dkgm(x, dkgm_radius = 2, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- dkgm_radius:

  Hop-distance radius \\R\\, a nonnegative number (default 2, a value
  the source recommends). `NULL` or `Inf` includes every reachable
  partner. `"auto"` uses half the mean finite hop distance, rounded to
  the nearest integer and at least one, following the source's equation
  4.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The k-shell peeling removes nodes of degree at most \\k\\, which is the
reading that terminates and reproduces the source's Tables 2 to 5, so an
isolate falls in the one-shell. Unreachable partners contribute nothing,
and isolates and a single-node graph score zero. The stage denominator
\\\max_k q(k) + 1\\ is a global maximum, so adding a disconnected
component can change every score. A `dkgm_radius` below one gives zero
at every node.

## References

Li, Z. and Huang, X. (2021). Identifying influential spreaders in
complex networks by an improved gravity model. Scientific Reports, 11,
22194.
[doi:10.1038/s41598-021-01218-1](https://doi.org/10.1038/s41598-021-01218-1)
.

## See also

[`centrality_mcgm`](https://sonsoles.me/cograph/reference/centrality_mcgm.md),
[`centrality_mixed_gravity`](https://sonsoles.me/cograph/reference/centrality_mixed_gravity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_dkgm(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>     580.80     726.30     875.56     716.58     557.89     588.00     452.23 
#>   Evaluate     Create      Share 
#>     603.84     737.64     617.40 
```
