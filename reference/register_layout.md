# Register a Custom Layout

Register a new layout algorithm that can be used for network
visualization.

## Usage

``` r
register_layout(name, layout_fn)
```

## Arguments

- name:

  Character. Name of the layout.

- layout_fn:

  Function. A function that computes node positions. Should accept a
  CographNetwork object and return a matrix with x, y columns.

## Value

Invisible NULL.

## Examples

``` r
# Register a simple random layout under a new name. Registering an existing
# name (for example "random") would replace the built-in layout for the rest
# of the session, so pick a name of your own.
register_layout("my_random", function(network, ...) {
  n <- network$n_nodes
  cbind(x = stats::runif(n), y = stats::runif(n))
})
```
