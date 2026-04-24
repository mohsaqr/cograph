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
#>   Nodes: 34  | Communities: 4  | Modularity: 0.4151 
#>   Sizes: 11, 5, 14, 4 
#> 
#>  node community
#>     1         1
#>     2         1
#>     3         1
#>     4         1
#>     5         2
#>     6         2
#>     7         2
#>     8         1
#>     9         3
#>    10         3
#>    11         2
#>    12         1
#>    13         1
#>    14         1
#>    15         3
#>    16         3
#>    17         2
#>    18         1
#>    19         3
#>    20         1
#>    21         3
#>    22         1
#>    23         3
#>    24         3
#>    25         4
#>    26         4
#>    27         3
#>    28         3
#>    29         4
#>    30         3
#>    31         3
#>    32         4
#>    33         3
#>    34         3
```
