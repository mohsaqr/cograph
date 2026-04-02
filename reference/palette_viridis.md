# Viridis Palette

Generate colors from the viridis palette.

## Usage

``` r
palette_viridis(n, alpha = 1, option = "viridis")
```

## Arguments

- n:

  Number of colors to generate.

- alpha:

  Transparency (0-1).

- option:

  Viridis option: "viridis", "magma", "plasma", "inferno", "cividis".

## Value

Character vector of colors.

## Examples

``` r
palette_viridis(5)
#> [1] "#440154" "#3B5188" "#269089" "#63C65D" "#FDE725"
```
