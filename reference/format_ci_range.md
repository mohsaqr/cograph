# Format CI Range

Formats confidence interval bounds as a range string.

## Usage

``` r
format_ci_range(
  lower,
  upper,
  digits = 2,
  format = "bracket",
  leading_zero = TRUE
)
```

## Arguments

- lower:

  Lower bound.

- upper:

  Upper bound.

- digits:

  Number of decimal places.

- format:

  CI format: "bracket" for `[low, up]` or "dash" for `low-up`.

- leading_zero:

  Logical: include leading zero for values \< 1? Default TRUE.

## Value

Formatted CI range string.
