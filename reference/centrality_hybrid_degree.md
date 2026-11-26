# Hybrid Degree Centrality

Hybrid degree centrality (HC; Ma and Ma 2017) blends the degree of a
node with its modified local centrality, in a proportion set by the
spreading probability \\p\\ of the process the ranking is meant for:
\$\$HC(v) = (\beta - p)\\\alpha\\k_v + p\\MLC(v),\$\$ with \\k_v\\ the
degree and \\MLC\\ the score
[`centrality_mlc`](https://sonsoles.me/cograph/reference/centrality_mlc.md)
returns. A low \\p\\ favors the degree and a high \\p\\ the wider
neighborhood.

## Usage

``` r
centrality_hybrid_degree(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `hc_p` (the spreading probability, required),
  `hc_alpha` (default 1000) and `hc_beta` (default 0.1).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
`hc_p` must be given: the source varies it between 0.01 and 0.1 and
names no default. Without it the measure warns (class
`cograph_undefined_measure`) and returns `NA`. `hc_alpha` brings the
degree to the scale of the modified local centrality and is 1000 by
default, the value the source uses throughout. `hc_beta` is 0.1 by
default; the source tunes it per network between 0.1 and 0.2. With the
defaults the degree term dominates on a small network, and the score
ranks nodes much like the degree. At `hc_beta = hc_p` the score is
\\p\\MLC\\, and above that the degree enters with a negative sign, a
case the source does not use. Isolated nodes score 0. The source prints
no value of the measure, so the implementation is checked against the
definition only. A `hc_p` outside \\\[0, 1\]\\, a negative `hc_alpha` or
a non-finite `hc_beta` raises a `cograph_bad_parameter` error.
`normalized = TRUE` divides the scores by their maximum.

## References

Ma, Q. and Ma, J. (2017). Identifying and ranking influential spreaders
in complex networks with consideration of spreading probability. Physica
A: Statistical Mechanics and its Applications, 465, 312-330.
[doi:10.1016/j.physa.2016.08.041](https://doi.org/10.1016/j.physa.2016.08.041)
.

## See also

[`centrality_mlc`](https://sonsoles.me/cograph/reference/centrality_mlc.md),
[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_hybrid_degree(regulation_net, hc_p = 0.05)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>     259.45     311.20     362.95     310.85     259.10     259.80     208.40 
#>   Evaluate     Create      Share 
#>     260.50     311.55     260.50 
```
