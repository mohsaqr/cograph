# Extract Layer from Supra-Adjacency Matrix

Extract Layer from Supra-Adjacency Matrix

## Usage

``` r
supra_layer(x, layer)

extract_layer(x, layer)
```

## Arguments

- x:

  Supra-adjacency matrix

- layer:

  Layer index to extract

## Value

Intra-layer adjacency matrix

## Examples

``` r
if (FALSE) { # \dontrun{
S <- supra_adjacency(layers, omega = 0.5)
supra_layer(S, 1)
} # }
if (FALSE) { # \dontrun{
S <- supra_adjacency(layers, omega = 0.5)
extract_layer(S, 2)
} # }
```
