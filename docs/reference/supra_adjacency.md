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
# layers <- list(L1 = mat1, L2 = mat2)
# S <- supra_adjacency(layers, omega = 0.5)
# dim(S)  # (2*n) x (2*n)
```
