# Get a Registered Shape

Get a Registered Shape

## Usage

``` r
get_shape(name)
```

## Arguments

- name:

  Character. Name of the shape.

## Value

The shape drawing function, or NULL if not found.

## Examples

``` r
get_shape("circle")
#> function (x, y, size, fill, border_color, border_width, alpha = 1, 
#>     ...) 
#> {
#>     fill_col <- adjust_alpha(fill, alpha)
#>     border_col <- adjust_alpha(border_color, alpha)
#>     grid::circleGrob(x = grid::unit(x, "npc"), y = grid::unit(y, 
#>         "npc"), r = grid::unit(size, "npc"), gp = grid::gpar(fill = fill_col, 
#>         col = border_col, lwd = border_width))
#> }
#> <bytecode: 0x56312dee17d8>
#> <environment: namespace:cograph>
```
