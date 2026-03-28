# Extract Layer from Supra-Adjacency Matrix

Extract Layer from Supra-Adjacency Matrix

## Usage

``` r
supra_layer(x, layer)

extract_layer(x, layer)
```

## Arguments

- x:

  Supra-adjacency matrix

- layer:

  Layer index to extract

## Value

Intra-layer adjacency matrix

## Examples

``` r
L1 <- matrix(c(0,.5,.3,.5,0,.4,.3,.4,0), 3, 3)
L2 <- matrix(c(0,.2,.6,.2,0,.1,.6,.1,0), 3, 3)
S <- supra_adjacency(list(L1 = L1, L2 = L2), omega = 0.5)
supra_layer(S, 1)
#>     1   2   3
#> 1 0.0 0.5 0.3
#> 2 0.5 0.0 0.4
#> 3 0.3 0.4 0.0
L1 <- matrix(c(0,.5,.3,.5,0,.4,.3,.4,0), 3, 3)
L2 <- matrix(c(0,.2,.6,.2,0,.1,.6,.1,0), 3, 3)
S <- supra_adjacency(list(L1 = L1, L2 = L2), omega = 0.5)
extract_layer(S, 2)
#>     1   2   3
#> 1 0.0 0.2 0.6
#> 2 0.2 0.0 0.1
#> 3 0.6 0.1 0.0
```
