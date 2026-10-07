# CographLayout R6 Class

Class for managing layout algorithms and computing node positions.

## Value

A `CographLayout` R6 object.

## Methods

### Public methods

- [`CographLayout$new()`](#method-CographLayout-new)

- [`CographLayout$compute()`](#method-CographLayout-compute)

- [`CographLayout$normalize_coords()`](#method-CographLayout-normalize_coords)

- [`CographLayout$get_type()`](#method-CographLayout-get_type)

- [`CographLayout$get_params()`](#method-CographLayout-get_params)

- [`CographLayout$print()`](#method-CographLayout-print)

- [`CographLayout$clone()`](#method-CographLayout-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new CographLayout object.

#### Usage

    CographLayout$new(type = "circle", ...)

#### Arguments

- `type`:

  Layout name. One of the names returned by
  [`list_layouts()`](https://sonsoles.me/cograph/reference/layout_registry.md),
  or `"custom"` together with a `coords` argument.

- `...`:

  Additional parameters stored and passed to the layout function.

#### Returns

A new CographLayout object.

------------------------------------------------------------------------

### Method `compute()`

Compute layout coordinates for a network.

#### Usage

    CographLayout$compute(network, ...)

#### Arguments

- `network`:

  A CographNetwork or cograph_network object.

- `...`:

  Additional parameters passed to the layout function. They override
  parameters given to `$new()`.

#### Returns

A data frame with columns `x` and `y`, one row per node, rescaled by
`$normalize_coords()`.

------------------------------------------------------------------------

### Method `normalize_coords()`

Rescale coordinates into the unit square. Both axes are scaled by the
same factor, so the larger spread spans `[padding, 1 - padding]` and the
layout is centered at 0.5.

#### Usage

    CographLayout$normalize_coords(coords, padding = 0.1)

#### Arguments

- `coords`:

  Matrix or data frame. Columns `x` and `y` are used, or the first two
  columns when these names are absent.

- `padding`:

  Numeric. Margin left on each side of the larger spread.

#### Returns

A data frame with rescaled `x` and `y` columns.

------------------------------------------------------------------------

### Method `get_type()`

Get layout type.

#### Usage

    CographLayout$get_type()

#### Returns

A character string.

------------------------------------------------------------------------

### Method `get_params()`

Get layout parameters.

#### Usage

    CographLayout$get_params()

#### Returns

A list of the parameters given to `$new()`.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print layout summary.

#### Usage

    CographLayout$print()

#### Returns

The object itself, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CographLayout$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
layout <- CographLayout$new("circle")
layout$compute(CographNetwork$new(regulation_net))
#>            x         y
#> 1  0.7351141 0.8236068
#> 2  0.8804226 0.6236068
#> 3  0.8804226 0.3763932
#> 4  0.7351141 0.1763932
#> 5  0.5000000 0.1000000
#> 6  0.2648859 0.1763932
#> 7  0.1195774 0.3763932
#> 8  0.1195774 0.6236068
#> 9  0.2648859 0.8236068
#> 10 0.5000000 0.9000000
```
