# Map equation centrality with explicit coding and flow conventions

Measures the reduction in codelength when a node is silenced, comparing
the original codebook used without that node's codeword to a redesigned
codebook. It does not remove the node or recompute the network
partition. The score in bits is -(s-p) log2((s-p)/s), where p is the
node visit rate and s is the rate of use of its module's codebook.

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

  One module label per node; NULL gives one module. Unnamed vectors
  follow input order. Named vectors must match all input node names
  exactly and are reordered to input order.

- map_flow:

  `"unrecorded"` (default) for unrecorded link teleportation, or
  `"recorded"` for recorded uniform node teleportation.

- map_convention:

  `"paper"` (default) includes the exit symbol; `"infomap"` reproduces
  the visit-only author implementation/table.

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  including `damping`, `weighted`, `simplify`, and `normalized`.

## Value

Named numeric vector in input node order.

## Details

With `map_convention = "paper"`, s includes module node visits and
module exits, as explicitly defined in Blocker et al. (2022), equations
2 and 9-11. With `"infomap"`, s includes node visits only, reproducing
Infomap 2.15.1's modular centrality and the paper's Table 1. These two
conventions differ when a module has exit flow. The published table does
not reproduce the equation's exit-inclusive convention. Both conventions
give nonnegative scores; the continuous boundary value is zero if p or
s-p is zero. The Zoo summary uses a different codelength subtraction.

The default unrecorded link-teleportation model teleports proportionally
to out-strength, then records only link-following steps and normalizes
their total flow to one. On undirected inputs this gives visit rates
proportional to strength, independent of damping. Recorded node
teleportation uses uniform destinations and records all moves, including
teleportation. Both models teleport away from dangling nodes. Damping is
the probability of following a link, default 0.85; it must be less than
one. Recorded teleportation remains directed even for reciprocal input
arcs.

Weights are nonnegative interaction strengths; zero weights are absent.
Direction is retained, and undirected edges become reciprocal arcs.
Loops are removed. Parallel edges follow centrality's simplify rule;
remaining parallel weights sum. Mode, inversion and cutoff are ignored.
With no positive edges, unrecorded flow and scores are zero by cograph
convention; recorded flow is uniform. Empty and singleton graphs return
no scores and zero respectively. Unrecorded isolates score zero;
recorded isolates may have positive scores because their teleportation
visits are recorded.

The partition is held fixed. NULL means one module containing every
node, the paper's one-level case. For a hierarchical partition, supply
globally unique leaf-module labels: silencing affects only that leaf
codebook, so higher levels cancel in the score difference. This function
does not run community detection or claim that a supplied partition is
optimal.

Dense flow calculation takes O(n cubed) time and O(n squared) memory;
unrecorded undirected flow takes O(n squared). Extreme weight ranges or
numerically singular flow solves raise errors. Optional maximum scaling
changes the raw bit units; tiny relative scores may underflow.

## References

Blocker, C., Nieves, J. C. and Rosvall, M. (2022). Map equation
centrality: community-aware centrality based on the map equation.
Applied Network Science, 7, 56.
[doi:10.1007/s41109-022-00477-9](https://doi.org/10.1007/s41109-022-00477-9)
. Lambiotte, R. and Rosvall, M. (2012). Ranking and clustering of nodes
in networks with smart teleportation. Physical Review E, 85, 056107.
[doi:10.1103/PhysRevE.85.056107](https://doi.org/10.1103/PhysRevE.85.056107)
.

## Examples

``` r
g <- igraph::make_graph("Zachary")
centrality_map_equation(g)
#>          1          2          3          4          5          6          7 
#> 0.14010698 0.08078392 0.08945089 0.05440724 0.02747564 0.03651382 0.03651382 
#>          8          9         10         11         12         13         14 
#> 0.03651382 0.04549115 0.01837702 0.02747564 0.00921834 0.01837702 0.04549115 
#>         15         16         17         18         19         20         21 
#> 0.01837702 0.01837702 0.01837702 0.01837702 0.01837702 0.02747564 0.01837702 
#>         22         23         24         25         26         27         28 
#> 0.01837702 0.01837702 0.04549115 0.02747564 0.02747564 0.01837702 0.03651382 
#>         29         30         31         32         33         34 
#> 0.02747564 0.03651382 0.03651382 0.05440724 0.10659435 0.14832115 
centrality_map_equation(g, membership = rep(1:2, each = 17),
                        map_convention = "infomap")
#>           1           2           3           4           5           6 
#> 0.132073065 0.078364418 0.086443304 0.053353397 0.027217306 0.036051565 
#>           7           8           9          10          11          12 
#> 0.036051565 0.036051565 0.044764137 0.018262938 0.027217306 0.009190003 
#>          13          14          15          16          17          18 
#> 0.018262938 0.044764137 0.018262938 0.018262938 0.018262938 0.018250557 
#>          19          20          21          22          23          24 
#> 0.018250557 0.027189203 0.018250557 0.018250557 0.018250557 0.044684666 
#>          25          26          27          28          29          30 
#> 0.027189203 0.027189203 0.018250557 0.036001159 0.027189203 0.036001159 
#>          31          32          33          34 
#> 0.036001159 0.053237915 0.101713852 0.138152458 
```
