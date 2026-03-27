# Colorblind-friendly Palette

Generate a colorblind-friendly palette using Wong's colors.

## Usage

``` r
palette_colorblind(n, alpha = 1)
```

## Arguments

- n:

  Number of colors to generate.

- alpha:

  Transparency (0-1).

## Value

Character vector of colors.

## Examples

``` r
palette_colorblind(5)
#> [1] "#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442"
```
