# Get a Registered Layout

Get a Registered Layout

## Usage

``` r
get_layout(name)
```

## Arguments

- name:

  Character. Name of the layout.

## Value

The layout function, or NULL if not found.

## Examples

``` r
get_layout("circle")
#> function (network, order = NULL, start_angle = pi/2, clockwise = TRUE, 
#>     ...) 
#> {
#>     n <- if (inherits(network, "cograph_network")) {
#>         n_nodes(network)
#>     }
#>     else {
#>         network$n_nodes
#>     }
#>     if (n == 0) {
#>         return(data.frame(x = numeric(0), y = numeric(0)))
#>     }
#>     if (n == 1) {
#>         return(data.frame(x = 0.5, y = 0.5))
#>     }
#>     if (!is.null(order)) {
#>         if (is.character(order)) {
#>             labels <- network$node_labels
#>             order <- match(order, labels)
#>             if (any(is.na(order))) {
#>                 warning("Some labels not found, using default order")
#>                 order <- seq_len(n)
#>             }
#>         }
#>         if (length(order) != n) {
#>             warning("Order length doesn't match node count, using default order")
#>             order <- seq_len(n)
#>         }
#>     }
#>     else {
#>         order <- seq_len(n)
#>     }
#>     angles <- seq(start_angle, start_angle + 2 * pi * (1 - 1/n), 
#>         length.out = n)
#>     if (clockwise) {
#>         angles <- rev(angles)
#>     }
#>     x <- 0.5 + 0.4 * cos(angles)
#>     y <- 0.5 + 0.4 * sin(angles)
#>     coords <- data.frame(x = x, y = y)
#>     coords[order, ] <- coords
#>     coords
#> }
#> <bytecode: 0x56312e003a90>
#> <environment: namespace:cograph>
```
