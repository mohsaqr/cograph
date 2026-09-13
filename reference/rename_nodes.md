# Rename Nodes

Rename Nodes

## Usage

``` r
rename_nodes(x, from, to = NULL, keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input.

- from:

  Character vector of current labels, or a named character vector
  mapping old label to new (in which case `to` is not used).

- to:

  Character vector of new labels, the same length as `from`.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` with the renamed nodes, or the input format when
`keep_format = TRUE`. Labels not named in `from` are left alone.

## See also

[`reorder_nodes`](https://sonsoles.me/cograph/reference/reorder_nodes.md),
[`set_nodes`](https://sonsoles.me/cograph/reference/set_nodes.md)

## Examples

``` r
adj <- matrix(c(0, 1, 1, 0), 2, 2)
rownames(adj) <- colnames(adj) <- c("A", "B")

get_labels(rename_nodes(adj, from = "A", to = "Alpha"))
#> [1] "Alpha" "B"    
get_labels(rename_nodes(adj, from = c(A = "Alpha", B = "Beta")))
#> [1] "Alpha" "Beta" 
```
