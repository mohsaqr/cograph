# Detect Core-Periphery Structure

Identifies core-periphery structure in a network using either continuous
(Borgatti-Everett) or discrete methods. Core nodes are densely
interconnected, while periphery nodes connect primarily to the core.

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

  Integer; maximum number of iterations for the continuous algorithm.
  Default 100.

- digits:

  Integer or NULL. Round numeric outputs to this many decimal places.
  Default NULL (no rounding).

- ...:

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md)

## Value

A data frame with class `"cograph_core_periphery"` and columns `node`,
`role`, and `coreness`. Fitness, core density, periphery density, and
the original network are stored as attributes.

## Details

**Continuous method (Borgatti-Everett):** Finds a coreness vector `c`
(values 0-1) that maximizes the correlation between the adjacency matrix
and the ideal rank-1 pattern matrix (the outer product of the coreness
vector with itself). The algorithm initializes from eigenvector
centrality and iteratively refines via power iteration until
convergence.

**Discrete method:** Produces a binary core (1) / periphery (0)
assignment. Starts from the continuous solution, thresholds at the
median, then greedily swaps node assignments to maximize fitness.
Discrete fitness is defined by high density within the core and low
density within the periphery.

## References

Borgatti, S.P. & Everett, M.G. (2000). Models of core/periphery
structures. *Social Networks*, 21(4), 375-395.
[doi:10.1016/S0378-8733(99)00019-2](https://doi.org/10.1016/S0378-8733%2899%2900019-2)

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
[`network_summary`](https://sonsoles.me/cograph/reference/network_summary.md)

## Examples

``` r
# Core-periphery in a simple network
adj <- matrix(c(
  0, 1, 1, 1, 0,
  1, 0, 1, 1, 0,
  1, 1, 0, 1, 1,
  1, 1, 1, 0, 1,
  0, 0, 1, 1, 0
), 5, 5)
rownames(adj) <- colnames(adj) <- LETTERS[1:5]
cp <- cograph::core_periphery(adj)
cp
#> Core-Periphery | Core: 4  Periphery: 1  Fitness: 0.569
#> Core density: 1.000 | Periphery density: 0.000
#> 
#>  node      role  coreness
#>     A      core 0.6504481
#>     B      core 0.6504481
#>     C      core 1.0000000
#>     D      core 1.0000000
#>     E periphery 0.0000000

# Discrete assignment
cp_disc <- cograph::core_periphery(adj, method = "discrete")
cp_disc$assignment
#> NULL
```
