# Expand Parameter to Length (Strict)

Expands a parameter to length n. Only accepts length 1 or length n.
Throws error for any other length (no silent recycling).

## Usage

``` r
expand_param(x, n, name = "parameter")
```

## Arguments

- x:

  Value to expand.

- n:

  Target length.

- name:

  Parameter name for error message.

## Value

Vector of length n.
