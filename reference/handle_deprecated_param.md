# Handle Deprecated Parameter

Handles backwards compatibility for renamed parameters. If the old
parameter name is used (not NULL), issues a deprecation warning and
returns the old value. Otherwise returns the new parameter value.

## Usage

``` r
handle_deprecated_param(
  new_val,
  old_val,
  new_name,
  old_name,
  new_val_was_set = NULL
)
```

## Arguments

- new_val:

  The value of the new parameter name.

- old_val:

  The value of the old (deprecated) parameter name.

- new_name:

  Character string of the new parameter name (for warning message).

- old_name:

  Character string of the old parameter name (for warning message).

- new_val_was_set:

  Logical. TRUE if the user explicitly set new_val (FALSE means it's
  just the default). When NULL, the function checks if new_val is NULL
  to determine this.

## Value

The effective parameter value.

## Details

For parameters with defaults, use `new_val_was_set` to indicate whether
the user explicitly provided the new value. If FALSE (user didn't set
it) and old_val is provided, the old value takes precedence.
