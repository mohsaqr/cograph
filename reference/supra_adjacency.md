# Supra-Adjacency Matrix

Builds the supra-adjacency matrix for multilayer networks. Diagonal
blocks = intra-layer, off-diagonal = inter-layer.

## Usage

``` r
supra_adjacency(
  layers,
  omega = 1,
  coupling = c("diagonal", "full", "custom"),
  interlayer_matrices = NULL
)

supra(
  layers,
  omega = 1,
  coupling = c("diagonal", "full", "custom"),
  interlayer_matrices = NULL
)
```

## Arguments

- layers:

  List of adjacency matrices (same dimensions)

- omega:

  Inter-layer coupling coefficient (scalar or L x L matrix)

- coupling:

  Coupling type: "diagonal", "full", or "custom"

- interlayer_matrices:

  For `coupling = "custom"`, a list of inter-layer matrices. Accepted
  shapes:

  - Named list with keys `"a_b"` (integer layer indices) or
    `"<layer_name_a>_<layer_name_b>"`; either order works.

  - Unnamed list of length `choose(L, 2)` giving every pair in
    upper-triangle row-major order:
    `(1,2), (1,3), ..., (1,L), (2,3), ..., (L-1,L)`.

  - Unnamed list of length `L-1` giving adjacent pairs only (legacy
    chain layout): entry `i` is the coupling for `(i, i+1)`.
    Non-adjacent pairs use `omega[a,b] * I`.

  If no entry matches a pair and no legacy chain layout applies, a
  warning is emitted and the diagonal default `omega[a,b] * I` is used
  (previously this happened silently).

## Value

Supra-adjacency matrix of dimension (N*L) x (N*L)

## Examples

``` r
nodes <- c("A", "B", "C")
l1 <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, dimnames = list(nodes, nodes))
l2 <- matrix(c(0, 1, 1, 1, 0, 0, 1, 0, 0), 3, 3, dimnames = list(nodes, nodes))
layers <- list(L1 = l1, L2 = l2)

# 3 nodes x 2 layers gives a 6 x 6 supra-adjacency matrix.
s <- supra_adjacency(layers, omega = 0.5)
dim(s)
#> [1] 6 6
s
#>      L1_A L1_B L1_C L2_A L2_B L2_C
#> L1_A  0.0  1.0  0.0  0.5  0.0  0.0
#> L1_B  1.0  0.0  1.0  0.0  0.5  0.0
#> L1_C  0.0  1.0  0.0  0.0  0.0  0.5
#> L2_A  0.5  0.0  0.0  0.0  1.0  1.0
#> L2_B  0.0  0.5  0.0  1.0  0.0  0.0
#> L2_C  0.0  0.0  0.5  1.0  0.0  0.0
#> attr(,"n_nodes")
#> [1] 3
#> attr(,"n_layers")
#> [1] 2
#> attr(,"node_names")
#> [1] "A" "B" "C"
#> attr(,"layer_names")
#> [1] "L1" "L2"
#> attr(,"omega")
#> [1] 0.5
#> attr(,"coupling")
#> [1] "diagonal"
#> attr(,"class")
#> [1] "supra_adjacency" "matrix"         
```
