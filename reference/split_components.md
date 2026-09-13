# Split a Network into Its Connected Components

Split a Network into Its Connected Components

## Usage

``` r
split_components(x, min_size = 1L, keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input.

- min_size:

  Integer. Drop components smaller than this. Default 1 (keep all,
  including isolated nodes).

- keep_format:

  Logical. Return each component in the input format.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A list of `cograph_network` objects, one per component, ordered from
largest to smallest and named `"component_1"`, `"component_2"`, and so
on. Components are weakly connected, matching
`igraph::components(mode = "weak")`.

## See also

[`select_component`](https://sonsoles.me/cograph/reference/select_component.md),
[`remove_isolates`](https://sonsoles.me/cograph/reference/remove_isolates.md)

## Examples

``` r
adj <- matrix(0, 5, 5, dimnames = list(LETTERS[1:5], LETTERS[1:5]))
adj["A", "B"] <- adj["B", "A"] <- 1
adj["C", "D"] <- adj["D", "C"] <- 1

parts <- split_components(adj)
length(parts)
#> [1] 3
```
