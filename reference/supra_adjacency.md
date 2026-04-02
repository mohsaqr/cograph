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

  For coupling="custom", list of inter-layer matrices

## Value

Supra-adjacency matrix of dimension (N*L) x (N*L)

## Examples

``` r
# layers <- list(L1 = mat1, L2 = mat2)
# S <- supra_adjacency(layers, omega = 0.5)
# dim(S)  # (2*n) x (2*n)
```
