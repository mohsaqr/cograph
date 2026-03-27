# Extract Inter-Layer Block

Extract Inter-Layer Block

## Usage

``` r
supra_interlayer(x, from, to)

extract_interlayer(x, from, to)
```

## Arguments

- x:

  Supra-adjacency matrix

- from:

  Source layer index

- to:

  Target layer index

## Value

Inter-layer adjacency matrix

## Examples

``` r
if (FALSE) { # \dontrun{
S <- supra_adjacency(layers, omega = 0.5)
supra_interlayer(S, 1, 2)
} # }
if (FALSE) { # \dontrun{
S <- supra_adjacency(layers, omega = 0.5)
extract_interlayer(S, 1, 2)
} # }
```
