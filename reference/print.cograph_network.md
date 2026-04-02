# Print cograph_network Object

Print cograph_network Object

## Usage

``` r
# S3 method for class 'cograph_network'
print(x, ...)
```

## Arguments

- x:

  A cograph_network object.

- ...:

  Ignored.

## Value

The input object `x`, invisibly.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- cograph(adj)
print(net)
#> Cograph network: 3 nodes, 3 edges ( undirected )
#> Source: matrix 
#>   Nodes (3): 1, 2, 3
#>   Edges: 3 / 3 (density: 100.0%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     1 -- 2  1.000
#>     1 -- 3  1.000
#>     2 -- 3  1.000
#> Layout: none 
```
