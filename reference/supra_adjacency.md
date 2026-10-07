# Supra-Adjacency Matrix

Builds the supra-adjacency matrix of a multilayer network. The diagonal
blocks hold the intra-layer adjacencies and the off-diagonal blocks the
inter-layer coupling.

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

  Inter-layer coupling coefficient, a scalar or an L x L matrix.
  Default 1. For a matrix, entry `[a, b]` with `a < b` sets the coupling
  of layers a and b.

- coupling:

  Coupling type. `"diagonal"` (default) couples each node to its own
  copy in the other layers with weight `omega`. `"full"` couples every
  node to every node of the other layers with weight `omega`. `"custom"`
  uses `interlayer_matrices`.

- interlayer_matrices:

  For `coupling = "custom"`, a list of inter-layer matrices. Accepted
  shapes:

  - Named list with keys `"a_b"` (integer layer indices) or
    `"<layer_name_a>_<layer_name_b>"`; either order works.

  - Unnamed list of length `choose(L, 2)` giving every pair in
    upper-triangle row-major order:
    `(1,2), (1,3), ..., (1,L), (2,3), ..., (L-1,L)`.

  - Unnamed list of length `L-1` giving adjacent pairs only. Entry `i`
    is the coupling for `(i, i+1)`.

  The block of layers b and a is the transpose of the block of a and b.
  A pair with no matching entry receives the diagonal coupling
  `omega[a,b] * I` with a warning. A `NULL` value with
  `coupling = "custom"` is an error.

## Value

A supra-adjacency matrix of dimension (N*L) x (N*L) with class
`c("supra_adjacency", "matrix")`. Diagonal N x N blocks hold the
intra-layer adjacencies and off-diagonal blocks the inter-layer
coupling. The attributes `"n_nodes"`, `"n_layers"`, `"node_names"`,
`"layer_names"`, `"omega"` and `"coupling"` record the construction and
are read back by
[`supra_layer()`](https://sonsoles.me/cograph/reference/supra_layer.md)
and
[`supra_interlayer()`](https://sonsoles.me/cograph/reference/supra_interlayer.md).

## Examples

``` r
layers <- list(forward = regulation_net, backward = t(regulation_net))
supra_adjacency(layers, omega = 0.5)
#>                     forward_Explore forward_Plan forward_Monitor forward_Adapt
#> forward_Explore                0.00         0.00            0.00          0.00
#> forward_Plan                   0.00         0.00            0.13          0.00
#> forward_Monitor                0.00         0.00            0.00          0.16
#> forward_Adapt                  0.28         0.00            0.00          0.00
#> forward_Reflect                0.05         0.00            0.15          0.00
#> forward_Discuss                0.30         0.00            0.00          0.00
#> forward_Synthesize             0.00         0.11            0.07          0.00
#> forward_Evaluate               0.00         0.00            0.33          0.43
#> forward_Create                 0.14         0.00            0.17          0.00
#> forward_Share                  0.00         0.21            0.49          0.39
#> backward_Explore               0.50         0.00            0.00          0.00
#> backward_Plan                  0.00         0.50            0.00          0.00
#> backward_Monitor               0.00         0.00            0.50          0.00
#> backward_Adapt                 0.00         0.00            0.00          0.50
#> backward_Reflect               0.00         0.00            0.00          0.00
#> backward_Discuss               0.00         0.00            0.00          0.00
#> backward_Synthesize            0.00         0.00            0.00          0.00
#> backward_Evaluate              0.00         0.00            0.00          0.00
#> backward_Create                0.00         0.00            0.00          0.00
#> backward_Share                 0.00         0.00            0.00          0.00
#>                     forward_Reflect forward_Discuss forward_Synthesize
#> forward_Explore                0.35            0.00               0.00
#> forward_Plan                   0.00            0.40               0.00
#> forward_Monitor                0.00            0.00               0.00
#> forward_Adapt                  0.00            0.34               0.17
#> forward_Reflect                0.00            0.00               0.00
#> forward_Discuss                0.35            0.00               0.00
#> forward_Synthesize             0.42            0.00               0.00
#> forward_Evaluate               0.07            0.00               0.00
#> forward_Create                 0.00            0.00               0.00
#> forward_Share                  0.00            0.00               0.00
#> backward_Explore               0.00            0.00               0.00
#> backward_Plan                  0.00            0.00               0.00
#> backward_Monitor               0.00            0.00               0.00
#> backward_Adapt                 0.00            0.00               0.00
#> backward_Reflect               0.50            0.00               0.00
#> backward_Discuss               0.00            0.50               0.00
#> backward_Synthesize            0.00            0.00               0.50
#> backward_Evaluate              0.00            0.00               0.00
#> backward_Create                0.00            0.00               0.00
#> backward_Share                 0.00            0.00               0.00
#>                     forward_Evaluate forward_Create forward_Share
#> forward_Explore                 0.00           0.00          0.27
#> forward_Plan                    0.49           0.20          0.36
#> forward_Monitor                 0.00           0.37          0.00
#> forward_Adapt                   0.00           0.00          0.00
#> forward_Reflect                 0.00           0.00          0.00
#> forward_Discuss                 0.00           0.14          0.00
#> forward_Synthesize              0.00           0.00          0.00
#> forward_Evaluate                0.00           0.00          0.00
#> forward_Create                  0.39           0.00          0.23
#> forward_Share                   0.00           0.00          0.00
#> backward_Explore                0.00           0.00          0.00
#> backward_Plan                   0.00           0.00          0.00
#> backward_Monitor                0.00           0.00          0.00
#> backward_Adapt                  0.00           0.00          0.00
#> backward_Reflect                0.00           0.00          0.00
#> backward_Discuss                0.00           0.00          0.00
#> backward_Synthesize             0.00           0.00          0.00
#> backward_Evaluate               0.50           0.00          0.00
#> backward_Create                 0.00           0.50          0.00
#> backward_Share                  0.00           0.00          0.50
#>                     backward_Explore backward_Plan backward_Monitor
#> forward_Explore                 0.50          0.00             0.00
#> forward_Plan                    0.00          0.50             0.00
#> forward_Monitor                 0.00          0.00             0.50
#> forward_Adapt                   0.00          0.00             0.00
#> forward_Reflect                 0.00          0.00             0.00
#> forward_Discuss                 0.00          0.00             0.00
#> forward_Synthesize              0.00          0.00             0.00
#> forward_Evaluate                0.00          0.00             0.00
#> forward_Create                  0.00          0.00             0.00
#> forward_Share                   0.00          0.00             0.00
#> backward_Explore                0.00          0.00             0.00
#> backward_Plan                   0.00          0.00             0.00
#> backward_Monitor                0.00          0.13             0.00
#> backward_Adapt                  0.00          0.00             0.16
#> backward_Reflect                0.35          0.00             0.00
#> backward_Discuss                0.00          0.40             0.00
#> backward_Synthesize             0.00          0.00             0.00
#> backward_Evaluate               0.00          0.49             0.00
#> backward_Create                 0.00          0.20             0.37
#> backward_Share                  0.27          0.36             0.00
#>                     backward_Adapt backward_Reflect backward_Discuss
#> forward_Explore               0.00             0.00             0.00
#> forward_Plan                  0.00             0.00             0.00
#> forward_Monitor               0.00             0.00             0.00
#> forward_Adapt                 0.50             0.00             0.00
#> forward_Reflect               0.00             0.50             0.00
#> forward_Discuss               0.00             0.00             0.50
#> forward_Synthesize            0.00             0.00             0.00
#> forward_Evaluate              0.00             0.00             0.00
#> forward_Create                0.00             0.00             0.00
#> forward_Share                 0.00             0.00             0.00
#> backward_Explore              0.28             0.05             0.30
#> backward_Plan                 0.00             0.00             0.00
#> backward_Monitor              0.00             0.15             0.00
#> backward_Adapt                0.00             0.00             0.00
#> backward_Reflect              0.00             0.00             0.35
#> backward_Discuss              0.34             0.00             0.00
#> backward_Synthesize           0.17             0.00             0.00
#> backward_Evaluate             0.00             0.00             0.00
#> backward_Create               0.00             0.00             0.14
#> backward_Share                0.00             0.00             0.00
#>                     backward_Synthesize backward_Evaluate backward_Create
#> forward_Explore                    0.00              0.00            0.00
#> forward_Plan                       0.00              0.00            0.00
#> forward_Monitor                    0.00              0.00            0.00
#> forward_Adapt                      0.00              0.00            0.00
#> forward_Reflect                    0.00              0.00            0.00
#> forward_Discuss                    0.00              0.00            0.00
#> forward_Synthesize                 0.50              0.00            0.00
#> forward_Evaluate                   0.00              0.50            0.00
#> forward_Create                     0.00              0.00            0.50
#> forward_Share                      0.00              0.00            0.00
#> backward_Explore                   0.00              0.00            0.14
#> backward_Plan                      0.11              0.00            0.00
#> backward_Monitor                   0.07              0.33            0.17
#> backward_Adapt                     0.00              0.43            0.00
#> backward_Reflect                   0.42              0.07            0.00
#> backward_Discuss                   0.00              0.00            0.00
#> backward_Synthesize                0.00              0.00            0.00
#> backward_Evaluate                  0.00              0.00            0.39
#> backward_Create                    0.00              0.00            0.00
#> backward_Share                     0.00              0.00            0.23
#>                     backward_Share
#> forward_Explore               0.00
#> forward_Plan                  0.00
#> forward_Monitor               0.00
#> forward_Adapt                 0.00
#> forward_Reflect               0.00
#> forward_Discuss               0.00
#> forward_Synthesize            0.00
#> forward_Evaluate              0.00
#> forward_Create                0.00
#> forward_Share                 0.50
#> backward_Explore              0.00
#> backward_Plan                 0.21
#> backward_Monitor              0.49
#> backward_Adapt                0.39
#> backward_Reflect              0.00
#> backward_Discuss              0.00
#> backward_Synthesize           0.00
#> backward_Evaluate             0.00
#> backward_Create               0.00
#> backward_Share                0.00
#> attr(,"n_nodes")
#> [1] 10
#> attr(,"n_layers")
#> [1] 2
#> attr(,"node_names")
#>  [1] "Explore"    "Plan"       "Monitor"    "Adapt"      "Reflect"   
#>  [6] "Discuss"    "Synthesize" "Evaluate"   "Create"     "Share"     
#> attr(,"layer_names")
#> [1] "forward"  "backward"
#> attr(,"omega")
#> [1] 0.5
#> attr(,"coupling")
#> [1] "diagonal"
#> attr(,"class")
#> [1] "supra_adjacency" "matrix"         
```
