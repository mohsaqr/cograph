# Extract Inter-Layer Block

Extract Inter-Layer Block

## Usage

``` r
supra_interlayer(x, from, to)

extract_interlayer(x, from, to)
```

## Arguments

- x:

  Supra-adjacency matrix from
  [`supra_adjacency`](https://sonsoles.me/cograph/reference/supra_adjacency.md)

- from:

  Integer index of the source layer

- to:

  Integer index of the target layer

## Value

The N x N inter-layer block, with the supra-matrix labels
(`"<layer>_<node>"`) as dimnames. An index outside `1:L` raises an
error.

## Examples

``` r
layers <- list(forward = regulation_net, backward = t(regulation_net))
supra <- supra_adjacency(layers, omega = 0.5)
supra_interlayer(supra, from = 1, to = 2)
#>                    backward_Explore backward_Plan backward_Monitor
#> forward_Explore                 0.5           0.0              0.0
#> forward_Plan                    0.0           0.5              0.0
#> forward_Monitor                 0.0           0.0              0.5
#> forward_Adapt                   0.0           0.0              0.0
#> forward_Reflect                 0.0           0.0              0.0
#> forward_Discuss                 0.0           0.0              0.0
#> forward_Synthesize              0.0           0.0              0.0
#> forward_Evaluate                0.0           0.0              0.0
#> forward_Create                  0.0           0.0              0.0
#> forward_Share                   0.0           0.0              0.0
#>                    backward_Adapt backward_Reflect backward_Discuss
#> forward_Explore               0.0              0.0              0.0
#> forward_Plan                  0.0              0.0              0.0
#> forward_Monitor               0.0              0.0              0.0
#> forward_Adapt                 0.5              0.0              0.0
#> forward_Reflect               0.0              0.5              0.0
#> forward_Discuss               0.0              0.0              0.5
#> forward_Synthesize            0.0              0.0              0.0
#> forward_Evaluate              0.0              0.0              0.0
#> forward_Create                0.0              0.0              0.0
#> forward_Share                 0.0              0.0              0.0
#>                    backward_Synthesize backward_Evaluate backward_Create
#> forward_Explore                    0.0               0.0             0.0
#> forward_Plan                       0.0               0.0             0.0
#> forward_Monitor                    0.0               0.0             0.0
#> forward_Adapt                      0.0               0.0             0.0
#> forward_Reflect                    0.0               0.0             0.0
#> forward_Discuss                    0.0               0.0             0.0
#> forward_Synthesize                 0.5               0.0             0.0
#> forward_Evaluate                   0.0               0.5             0.0
#> forward_Create                     0.0               0.0             0.5
#> forward_Share                      0.0               0.0             0.0
#>                    backward_Share
#> forward_Explore               0.0
#> forward_Plan                  0.0
#> forward_Monitor               0.0
#> forward_Adapt                 0.0
#> forward_Reflect               0.0
#> forward_Discuss               0.0
#> forward_Synthesize            0.0
#> forward_Evaluate              0.0
#> forward_Create                0.0
#> forward_Share                 0.5
```
