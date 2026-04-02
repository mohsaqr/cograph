# Print Community Structure

Print Community Structure

## Usage

``` r
# S3 method for class 'cograph_communities'
print(x, ...)
```

## Arguments

- x:

  A cograph_communities object.

- ...:

  Ignored.

## Value

Invisibly returns the original object.

## Examples

``` r
g <- igraph::make_graph("Zachary")
comm <- community_louvain(g)
print(comm)
#> Community structure (louvain)
#>   Number of communities: 4 
#>   Modularity: 0.3952 
#>   Community sizes: 7, 10, 11, 6 
```
