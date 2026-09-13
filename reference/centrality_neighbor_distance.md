# Neighborhood centrality, and its neighbor distance special case

Neighborhood centrality adds to a node's own benchmark centrality the
benchmark centrality of the nodes its walks reach, discounted once per
step: \\C^n_i(\theta)=\theta_i+a\sum\_{j\in\Gamma_i}\theta_j
+a^2\sum\_{l\in\Gamma_j\setminus i}\theta_l+\dots
+a^n\sum\_{s\in\Gamma\_{s-1}\setminus x}\theta_s\\. The sums are nested
and each level excludes only the node the walk just came from, so the
\\k\\-th term sums \\\theta\\ over the endpoints of the
**non-backtracking walks of length \\k\\** that start at \\i\\, once per
walk. A walk may revisit a node it passed earlier, including \\i\\
itself; only immediate backtracking is barred. The Zoo calls the setting
`nd_mass = "degree"`, `nd_order = 2`, `nd_decay = 0.2` the *neighbor
distance centrality*, and that is the default here; it is the
configuration the source recommends.

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

  Number of steps \\n\\, a single nonnegative whole number; default two,
  the source's recommended setting. The source studies one to four
  steps. Zero returns the benchmark centrality.

- nd_decay:

  Per-step decay \\a\\, a single finite number; default 0.2, the
  source's own value. The source's domain is \\\[0,1\]\\.

- nd_mass:

  Benchmark centrality \\\theta\\: `"degree"` (default) or `"coreness"`.
  These are the two the source uses.

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in input node order.

## Details

**This is not the same as summing over distance shells.** The Centrality
Zoo (section 2.279, equation 2.1) paraphrases the measure with sums over
\\N^{(k)}(i)\\, "the set of \\k\\-hop neighbors", which visits each node
at most once per level and never revisits a closer one. The two readings
agree on trees and disagree on any graph carrying a triangle or a cycle
of length at most \\2n\\, and the difference is a per-node offset, not a
rescaling. On the triangle-plus-pendant `A-B, A-C, B-C, A-D` with the
defaults, the walk sums of the source give `4.16, 3.24, 3.24, 1.76`
while distance shells would give `4.00, 3.04, 3.04, 1.76`. cograph
implements the source equation. No shell variant is offered: the shell
form appears only in a secondary paraphrase, which also attributes the
measure to a different paper whose text does not contain it.

The source states no normalization, so raw scores grow with `nd_decay`
and `nd_order`; `normalized = TRUE` max-scales the finished vector and
is a cograph convention. `nd_decay` is \\a\in\[0,1\]\\ in the source,
which sweeps 0.1 to 0.5; cograph accepts any finite value, and a
negative or larger one leaves the source's domain. `nd_order = 0` drops
every sum and returns \\\theta\\ itself, which is what the source says
\\a=0\\ does.

Uses the simple undirected unweighted skeleton, which is the source
domain: either arc creates one edge, parallel edges count once, and
loops are removed, since a loop would make "the node the walk just came
from" ambiguous. Edge weights, mode, cutoff and path-weight inversion
are ignored. Isolates have every sum empty and score \\\theta_i\\, which
is zero for both benchmarks; walks never leave a component, so the raw
score of a node is unchanged by adding a disconnected component. Empty
graphs return no scores. Core numbers follow
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md)'s
`"coreness"`, so an isolate sits in the zero-shell. Cost is `nd_order`
dense matrix-vector products, O(n^2) each. Walk counts grow
geometrically in `nd_order`, so a large order overflows to infinity; the
source considers one to four steps.

Numerical verification establishes agreement with the source equation as
printed in the author preprint, not parity with author software, which
does not exist, and not any claim about spreading performance.

## References

Liu, Y., Tang, M., Zhou, T. and Do, Y. (2016). Identify influential
spreaders in complex networks, the role of neighborhood. Physica A:
Statistical Mechanics and its Applications, 452, 289-298. Section 2.3,
equation 1, read in the author preprint arXiv:1511.00441v1 page 4.
[doi:10.1016/j.physa.2016.02.028](https://doi.org/10.1016/j.physa.2016.02.028)
.

## See also

[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md)
and
[`centrality_extended_coreness`](https://sonsoles.me/cograph/reference/centrality_extended_coreness.md)
for other neighborhood sums, and
[`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)
for the catalogue.

## Examples

``` r
# Neighbor distance centrality: degree benchmark, two steps, a = 0.2
centrality_neighbor_distance(igraph::make_ring(6))
#>    1    2    3    4    5    6 
#> 2.96 2.96 2.96 2.96 2.96 2.96 

# The source's other benchmark, and a wider neighborhood
centrality_neighbor_distance(igraph::make_star(7, mode = "undirected"),
                             nd_order = 3, nd_mass = "coreness")
#>   1   2   3   4   5   6   7 
#> 2.2 1.4 1.4 1.4 1.4 1.4 1.4 
```
