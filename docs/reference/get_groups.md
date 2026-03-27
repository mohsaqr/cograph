# Get Node Groups from Cograph Network

Extracts the node groupings from a cograph_network object.

## Usage

``` r
get_groups(x)
```

## Arguments

- x:

  A cograph_network object.

## Value

A data frame with node groupings, or NULL if not set. The data frame has
columns:

- `node`: Node labels

- One of `layer`, `cluster`, or `group`: Group assignment

## See also

[`set_groups`](http://sonsoles.me/cograph/reference/set_groups.md),
[`splot`](http://sonsoles.me/cograph/reference/splot.md)

## Examples

``` r
mat <- matrix(runif(25), 5, 5)
rownames(mat) <- colnames(mat) <- LETTERS[1:5]
net <- as_cograph(mat)
net <- set_groups(net, list(G1 = c("A", "B"), G2 = c("C", "D", "E")))
get_groups(net)
#>   node group
#> 1    A    G1
#> 2    B    G1
#> 3    C    G2
#> 4    D    G2
#> 5    E    G2
```
