# Summary of cograph_network Object

Summary of cograph_network Object

## Usage

``` r
# S3 method for class 'cograph_network'
summary(object, ...)
```

## Arguments

- object:

  A cograph_network object.

- ...:

  Ignored.

## Value

A list with network summary information (invisibly), containing elements
`n_nodes`, `n_edges`, `directed`, `weighted`, and `has_layout`.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- cograph(adj)
summary(net)
#> Cograph Network Summary
#> ======================
#> 
#> Structure:
#>   Nodes: 3 
#>   Edges: 3 
#>   Type: Undirected 
#> 
#> Edge Statistics:
#>   Min weight: 1 
#>   Max weight: 1 
#>   Mean weight: 1 
#> 
#> Node Labels:
#>    1, 2, 3 
#> 
#> Layout: not computed 
```
