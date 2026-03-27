# Convert Fontface String to Numeric

Converts fontface string specification to numeric value used by R
graphics. Handles both string ("plain", "bold", "italic", "bold.italic")
and numeric (1, 2, 3, 4) inputs for backwards compatibility.

## Usage

``` r
fontface_to_numeric(fontface)
```

## Arguments

- fontface:

  Character or numeric fontface specification.

## Value

Numeric fontface value (1=plain, 2=bold, 3=italic, 4=bold.italic).
