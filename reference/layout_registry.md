# Layout Registry

Layout algorithms are stored in a registry and selected by name through
the `layout` argument of
[`splot`](https://sonsoles.me/cograph/reference/splot.md),
[`soplot`](https://sonsoles.me/cograph/reference/soplot.md) and
[`sn_layout`](https://sonsoles.me/cograph/reference/sn_layout.md).
`register_layout()` adds a layout function, `get_layout()` returns the
function of a registered layout, and `list_layouts()` returns the names
of all registered layouts. Registering an existing name replaces that
layout for the rest of the session.

## Usage

``` r
register_layout(name, layout_fn)

get_layout(name)

list_layouts()
```

## Arguments

- name:

  Character. The name of the layout.

- layout_fn:

  A function that computes node positions. It receives the network as
  the argument `network`, followed by any layout parameters, and returns
  a matrix or data frame with `x` and `y` columns.

## Value

`register_layout()` returns `NULL` invisibly. `get_layout()` returns the
layout function, or `NULL` if no layout has that name. `list_layouts()`
returns a character vector of layout names.

## See also

[`layout_circle`](https://sonsoles.me/cograph/reference/layout_circle.md),
[`layout_spring`](https://sonsoles.me/cograph/reference/layout_spring.md),
[`layout_groups`](https://sonsoles.me/cograph/reference/layout_groups.md),
[`layout_oval`](https://sonsoles.me/cograph/reference/layout_oval.md)

## Examples

``` r
list_layouts()
#>  [1] "circle"               "oval"                 "ellipse"             
#>  [4] "spring"               "fr"                   "fruchterman-reingold"
#>  [7] "target"               "saqr"                 "groups"              
#> [10] "grid"                 "random"               "star"                
#> [13] "bipartite"            "custom"               "gephi_fr"            
#> [16] "gephi"               
splot(regulation_net, layout = "circle")
```
