# Convert Network to ggplot2

Convert a Cograph network visualization to a ggplot2 object for further
customization and composability.

## Usage

``` r
sn_ggplot(network, title = NULL)
```

## Arguments

- network:

  A cograph_network object, matrix, data.frame, or igraph object.
  Matrices and other inputs are auto-converted.

- title:

  Optional plot title.

## Value

A ggplot2 object.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
# With cograph()
p <- cograph(adj) |> sn_ggplot()
print(p)


# Direct matrix input
p <- adj |> sn_ggplot()

# Further customization
p + ggplot2::labs(title = "My Network")
```
