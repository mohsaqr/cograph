# Build Edge Labels from Template

Generates edge labels for all edges using template formatting.

## Usage

``` r
build_edge_labels_from_template(
  template = NULL,
  style = "none",
  weights = NULL,
  ci_lower = NULL,
  ci_upper = NULL,
  p_values = NULL,
  stars = NULL,
  digits = 2,
  p_digits = 3,
  p_prefix = "p=",
  ci_format = "bracket",
  oneline = TRUE,
  leading_zero = TRUE,
  n
)
```

## Arguments

- template:

  Template string or NULL.

- style:

  Style preset (used if template is NULL).

- weights:

  Edge weights/estimates.

- ci_lower:

  Lower CI bounds (vector).

- ci_upper:

  Upper CI bounds (vector).

- p_values:

  P-values (vector).

- stars:

  Stars input (character vector, logical, or numeric p-values).

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

- n:

  Number of edges.

## Value

Character vector of formatted labels.
