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
#>   Nodes: 34  | Communities: 4  | Modularity: 0.3952 
#>   Sizes: 7, 10, 11, 6 
#> 
#>  node community
#>     1         1
#>     2         2
#>     3         2
#>     4         2
#>     5         1
#>     6         1
#>     7         1
#>     8         2
#>     9         3
#>    10         2
#>    11         1
#>    12         1
#>    13         2
#>    14         2
#>    15         3
#>    16         3
#>    17         1
#>    18         2
#>    19         3
#>    20         2
#>    21         3
#>    22         2
#>    23         3
#>    24         4
#>    25         4
#>    26         4
#>    27         3
#>    28         4
#>    29         4
#>    30         3
#>    31         3
#>    32         4
#>    33         3
#>    34         3
```
