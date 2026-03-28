# Extract Inter-Layer Block

Extract Inter-Layer Block

## Usage

``` r
supra_interlayer(x, from, to)

extract_interlayer(x, from, to)
```

## Arguments

- x:

  Supra-adjacency matrix

- from:

  Source layer index

- to:

  Target layer index

## Value

Inter-layer adjacency matrix

## Examples

``` r
L1 <- matrix(c(0,.5,.3,.5,0,.4,.3,.4,0), 3, 3)
L2 <- matrix(c(0,.2,.6,.2,0,.1,.6,.1,0), 3, 3)
S <- supra_adjacency(list(L1 = L1, L2 = L2), omega = 0.5)
supra_interlayer(S, 1, 2)
#>      L2_1 L2_2 L2_3
#> L1_1  0.5  0.0  0.0
#> L1_2  0.0  0.5  0.0
#> L1_3  0.0  0.0  0.5
L1 <- matrix(c(0,.5,.3,.5,0,.4,.3,.4,0), 3, 3)
L2 <- matrix(c(0,.2,.6,.2,0,.1,.6,.1,0), 3, 3)
S <- supra_adjacency(list(L1 = L1, L2 = L2), omega = 0.5)
extract_interlayer(S, 1, 2)
#>      L2_1 L2_2 L2_3
#> L1_1  0.5  0.0  0.0
#> L1_2  0.0  0.5  0.0
#> L1_3  0.0  0.0  0.5
```
