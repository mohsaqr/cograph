# Unregister SVG Shape

Remove a custom SVG shape from the registry.

## Usage

``` r
unregister_svg_shape(name)
```

## Arguments

- name:

  Shape name to remove.

## Value

Invisible TRUE if removed, FALSE if not found.

## Examples

``` r
# Attempt to unregister a non-existent shape (returns FALSE)
unregister_svg_shape("nonexistent")
```
