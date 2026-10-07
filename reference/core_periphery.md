# Detect Core-Periphery Structure

Identifies core-periphery structure in a network with a continuous
(Borgatti-Everett) or a discrete method. Core nodes are densely
interconnected, and periphery nodes connect mainly to the core. Edge
weights are ignored; the analysis uses the binary adjacency matrix.

## Usage

``` r
core_periphery(
  x,
  method = c("continuous", "discrete"),
  directed = NULL,
  iter = 100,
  digits = NULL,
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- method:

  Character string; either "continuous" (default, Borgatti-Everett
  model) or "discrete" (binary core/periphery assignment).

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected.

- iter:

  Integer; maximum number of power iterations. Must be at least

  1.  Default 100.

- digits:

  Integer or NULL. Number of decimal places for the coreness scores, the
  fitness and the densities. Default NULL (no rounding).

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  which accepts no further arguments. Any argument supplied here raises
  an error.

## Value

A data frame with class `"cograph_core_periphery"`, one row per node,
and columns:

- node:

  Node label.

- role:

  Character: `"core"` or `"periphery"`. For the continuous method a node
  is core when its coreness is at or above the median coreness.

- coreness:

  Numeric continuous coreness score, rescaled to \\\[0, 1\]\\. Reported
  for both methods.

The attributes `"fitness"`, `"core_density"` and `"periphery_density"`
hold the fit and the block densities, and `"network"` holds the original
input.

## Details

### Continuous method

The Borgatti-Everett model compares the adjacency matrix with the rank-1
pattern matrix `outer(c, c)` of a coreness vector `c`. Here `c` is
estimated from the dominant eigenvector of the adjacency matrix, refined
by power iteration with rescaling to \\\[0, 1\]\\. The iteration stops
when the largest change falls below \\10^{-6}\\ or after `iter` steps.
The vector is not optimized for the correlation. The `"fitness"`
attribute is the correlation between the off-diagonal entries of the
adjacency matrix and those of the pattern matrix (the lower triangle for
a symmetric matrix), and it is 0 when the correlation is undefined.

### Discrete method

The discrete method starts from the continuous solution split at the
median and repeatedly flips the single node assignment that most
increases `density(core) - density(periphery)`. It stops when no flip
increases this quantity. The `"fitness"` attribute is the correlation
between the adjacency matrix and the ideal block pattern of the final
assignment.

## Printing and plotting

Printing the result shows the core and periphery sizes, the fitness and
the two block densities, followed by the node table. The result is a
data frame and serves as the tidy table directly.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the result
is documented in
[`plot-results`](https://sonsoles.me/cograph/reference/plot-results.md).

## References

Borgatti, S.P. & Everett, M.G. (2000). Models of core/periphery
structures. *Social Networks*, 21(4), 375-395.
[doi:10.1016/S0378-8733(99)00019-2](https://doi.org/10.1016/S0378-8733%2899%2900019-2)

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
[`network_summary`](https://sonsoles.me/cograph/reference/network_summary.md)

## Examples

``` r
core_periphery(regulation_net)
#> Core-Periphery | Core: 5  Periphery: 5  Fitness: 0.030
#> Core density: 0.350 | Periphery density: 0.350
#> 
#>        node      role   coreness
#>     Explore periphery 0.07985633
#>        Plan      core 1.00000000
#>     Monitor periphery 0.00000000
#>       Adapt      core 0.44368868
#>     Reflect periphery 0.17972217
#>     Discuss periphery 0.05077436
#>  Synthesize      core 0.39000808
#>    Evaluate periphery 0.12894782
#>      Create      core 0.72952631
#>       Share      core 0.51706359
```
