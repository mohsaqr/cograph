# Plot cograph_network Object

Plot cograph_network Object

## Usage

``` r
# S3 method for class 'cograph_network'
plot(x, ...)
```

## Arguments

- x:

  A cograph_network object.

- ...:

  Additional arguments passed to sn_render.

## Value

The input object `x`, invisibly.

## Examples

``` r
# \donttest{
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- cograph(adj)
plot(net)

# }
```
