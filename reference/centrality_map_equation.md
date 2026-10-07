# Map Equation Centrality

Map equation centrality (Blocker et al. 2022) is the reduction in
codelength, in bits, when a node's codeword is removed from its module
codebook and the codebook is redesigned. With \\p\\ the visit rate of
the node and \\s\\ the use rate of its module codebook, \$\$MEC_i =
-(s - p) \log_2 \frac{s - p}{s}.\$\$

## Usage

``` r
centrality_map_equation(
  x,
  membership = NULL,
  map_flow = "unrecorded",
  map_convention = "paper",
  ...
)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- membership:

  One module label per node. `NULL` (default) gives one module. A named
  vector is matched to the node names.

- map_flow:

  `"unrecorded"` (default) for unrecorded link teleportation or
  `"recorded"` for recorded uniform teleportation.

- map_convention:

  `"paper"` (default) includes module exits in the codebook rate.
  `"infomap"` uses node visits only.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (use edge weights, default `TRUE`) and
  `damping` (probability of following a link, default 0.85).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The partition is held fixed and is given by `membership`, and `NULL`
places every node in one module. With `map_convention = "paper"` the
rate \\s\\ includes module exits (equations 2 and 9-11), and with
`"infomap"` it includes node visits only, which reproduces Infomap
2.15.1 and Table 1 of the paper. With `map_flow = "unrecorded"`
(Lambiotte and Rosvall 2012) the walk teleports in proportion to
out-strength and only link steps are recorded, so on an undirected
network the visit rates are proportional to strength for every
`damping`. With `"recorded"` the walk teleports uniformly and
teleportation steps are recorded. Direction is kept, loops are removed
and edge weights must be finite and nonnegative. Under unrecorded flow
an isolated node scores zero. An invalid `membership`, `map_flow` or
`map_convention`, or a `damping` outside \\\[0, 1)\\, raises an error.

## References

Blocker, C., Nieves, J. C. and Rosvall, M. (2022). Map equation
centrality: community-aware centrality based on the map equation.
Applied Network Science, 7, 56.
[doi:10.1007/s41109-022-00477-9](https://doi.org/10.1007/s41109-022-00477-9)
.

Lambiotte, R. and Rosvall, M. (2012). Ranking and clustering of nodes in
networks with smart teleportation. Physical Review E, 85, 056107.
[doi:10.1103/PhysRevE.85.056107](https://doi.org/10.1103/PhysRevE.85.056107)
.

## See also

[`centrality_community_based`](https://sonsoles.me/cograph/reference/centrality_community_based.md),
[`centrality_participation`](https://sonsoles.me/cograph/reference/centrality_participation.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_map_equation(regulation_net, membership = rep(1:2, each = 5))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.16113220 0.03673693 0.24678669 0.17278681 0.16904742 0.09194596 0.03809941 
#>   Evaluate     Create      Share 
#> 0.10074058 0.18236547 0.12807428 
```
