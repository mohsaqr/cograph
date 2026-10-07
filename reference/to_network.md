# Convert Network to statnet network Object

Converts any supported network format to a statnet network object.

## Usage

``` r
to_network(x, directed = NULL)
```

## Arguments

- x:

  Network input: matrix, cograph_network, igraph, tna, etc.

- directed:

  Logical or NULL. If NULL (default), auto-detect from input.

## Value

A network object from the network package.

## See also

[`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
[`to_matrix`](https://sonsoles.me/cograph/reference/to_matrix.md),
[`to_df`](https://sonsoles.me/cograph/reference/to_data_frame.md),
[`as_cograph`](https://sonsoles.me/cograph/reference/as_cograph.md)

## Examples

``` r
to_network(regulation_net)
#>  Network attributes:
#>   vertices = 10 
#>   directed = TRUE 
#>   hyper = FALSE 
#>   loops = FALSE 
#>   multiple = FALSE 
#>   bipartite = FALSE 
#>   total edges= 30 
#>     missing edges= 0 
#>     non-missing edges= 30 
#> 
#>  Vertex attribute names: 
#>     vertex.names 
#> 
#>  Edge attribute names: 
#>     weight 
```
