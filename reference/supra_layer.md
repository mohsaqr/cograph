# Extract Layer from Supra-Adjacency Matrix

Extract Layer from Supra-Adjacency Matrix

## Usage

``` r
supra_layer(x, layer)

extract_layer(x, layer)
```

## Arguments

- x:

  Supra-adjacency matrix from
  [`supra_adjacency`](https://sonsoles.me/cograph/reference/supra_adjacency.md)

- layer:

  Integer index of the layer to extract

## Value

The N x N intra-layer adjacency matrix, with the node names as dimnames.
An index outside `1:L` raises an error.

## Examples

``` r
layers <- list(forward = regulation_net, backward = t(regulation_net))
supra <- supra_adjacency(layers, omega = 0.5)
supra_layer(supra, layer = 2)
#>            Explore Plan Monitor Adapt Reflect Discuss Synthesize Evaluate
#> Explore       0.00 0.00    0.00  0.28    0.05    0.30       0.00     0.00
#> Plan          0.00 0.00    0.00  0.00    0.00    0.00       0.11     0.00
#> Monitor       0.00 0.13    0.00  0.00    0.15    0.00       0.07     0.33
#> Adapt         0.00 0.00    0.16  0.00    0.00    0.00       0.00     0.43
#> Reflect       0.35 0.00    0.00  0.00    0.00    0.35       0.42     0.07
#> Discuss       0.00 0.40    0.00  0.34    0.00    0.00       0.00     0.00
#> Synthesize    0.00 0.00    0.00  0.17    0.00    0.00       0.00     0.00
#> Evaluate      0.00 0.49    0.00  0.00    0.00    0.00       0.00     0.00
#> Create        0.00 0.20    0.37  0.00    0.00    0.14       0.00     0.00
#> Share         0.27 0.36    0.00  0.00    0.00    0.00       0.00     0.00
#>            Create Share
#> Explore      0.14  0.00
#> Plan         0.00  0.21
#> Monitor      0.17  0.49
#> Adapt        0.00  0.39
#> Reflect      0.00  0.00
#> Discuss      0.00  0.00
#> Synthesize   0.00  0.00
#> Evaluate     0.39  0.00
#> Create       0.00  0.00
#> Share        0.23  0.00
```
