# Neighborhood Centrality

Neighborhood centrality (Liu et al. 2016) adds to a node's benchmark
centrality \\\theta\\ the benchmark centrality of the endpoints of its
non-backtracking walks of length 1 to \\n\\, discounted by \\a^k\\ at
step \\k\\. With the defaults (degree benchmark, two steps, \\a = 0.2\\)
it is the neighbor distance centrality. \$\$C_i = \theta_i + a \sum\_{j
\in \Gamma_i} \theta_j + a^2 \sum\_{j \in \Gamma_i} \sum\_{l \in
\Gamma_j \setminus i} \theta_l + \dots\$\$

## Usage

``` r
centrality_neighbor_distance(
  x,
  nd_order = 2,
  nd_decay = 0.2,
  nd_mass = "degree",
  ...
)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- nd_order:

  Number of steps \\n\\, a nonnegative whole number (default 2).

- nd_decay:

  Per-step decay \\a\\, a finite number (default 0.2).

- nd_mass:

  Benchmark centrality \\\theta\\: `"degree"` (default) or `"coreness"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
Each level excludes only the node the walk came from, so a walk may
revisit a node. Isolates score \\\theta_i\\, which is zero for both
benchmarks, and `nd_order = 0` returns the benchmark itself. The source
takes \\a\\ in \\\[0, 1\]\\, and the function accepts any finite value.
The Centrality Zoo paraphrases the measure with sums over distance
shells, which agree with the source's walk sums on trees and differ on
graphs with short cycles. The implementation follows the source.

## References

Liu, Y., Tang, M., Zhou, T. and Do, Y. (2016). Identify influential
spreaders in complex networks, the role of neighborhood. Physica A:
Statistical Mechanics and its Applications, 452, 289-298.
[doi:10.1016/j.physa.2016.02.028](https://doi.org/10.1016/j.physa.2016.02.028)
.

## See also

[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md),
[`centrality_extended_coreness`](https://sonsoles.me/cograph/reference/centrality_extended_coreness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_neighbor_distance(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>      15.32      18.24      20.68      17.80      15.04      15.56      13.20 
#>   Evaluate     Create      Share 
#>      16.36      18.52      16.40 
```
