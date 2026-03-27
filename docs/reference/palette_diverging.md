# Diverging Palette

Generate a diverging color palette (blue-white-red).

## Usage

``` r
palette_diverging(n, alpha = 1, midpoint = "white")
```

## Arguments

- n:

  Number of colors to generate.

- alpha:

  Transparency (0-1).

- midpoint:

  Color for midpoint.

## Value

Character vector of colors.

## Examples

``` r
palette_diverging(5)
#> [1] "#2166AC" "#9CC7DF" "#FFFFFF" "#F6B294" "#B2182B"
```
