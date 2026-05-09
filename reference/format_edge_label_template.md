# Format Edge Label from Template

Processes a template string with placeholders and substitutes values.

## Usage

``` r
format_edge_label_template(
  template,
  weight = NA,
  ci_lower = NA,
  ci_upper = NA,
  p_value = NA,
  stars = "",
  digits = 2,
  p_digits = 3,
  p_prefix = "p=",
  ci_format = "bracket",
  oneline = TRUE,
  leading_zero = TRUE
)
```

## Arguments

- template:

  Template string with placeholders: {est}, {range}, {low}, {up}, {p},
  {stars}.

- weight:

  Edge weight (estimate).

- ci_lower:

  Lower CI bound.

- ci_upper:

  Upper CI bound.

- p_value:

  P-value.

- stars:

  Significance stars string.

- digits:

  Decimal places for estimates.

- p_digits:

  Decimal places for p-values.

- p_prefix:

  Prefix for p-values.

- ci_format:

  CI format: "bracket" or "dash".

- oneline:

  Logical. Accepted for compatibility; labels are currently normalized
  to single-line whitespace.

- leading_zero:

  Logical: include leading zero for values \< 1? Default TRUE.

## Value

Formatted label string.
