# Resolve Stars from Various Inputs

Resolves significance stars from character vectors, logical, or
p-values.

## Usage

``` r
resolve_stars(stars_input, p_values = NULL, n)
```

## Arguments

- stars_input:

  User input: character vector, logical, or numeric p-values.

- p_values:

  P-values for computing stars if stars_input is TRUE/numeric.

- n:

  Number of edges.

## Value

Character vector of stars.
