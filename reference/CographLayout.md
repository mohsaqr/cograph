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

  Layout type (e.g., "circle", "spring", "groups").

- `...`:

  Additional parameters for the layout algorithm.

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

  Additional parameters passed to the layout function.

#### Returns

Data frame with x, y coordinates.

------------------------------------------------------------------------

### Method `normalize_coords()`

Normalize coordinates to 0-1 range with padding.

#### Usage

    CographLayout$normalize_coords(coords, padding = 0.1)

#### Arguments

- `coords`:

  Matrix or data frame with x, y columns.

- `padding`:

  Numeric. Padding around edges (default 0.1).

#### Returns

Normalized coordinates.

------------------------------------------------------------------------

### Method `get_type()`

Get layout type.

#### Usage

    CographLayout$get_type()

#### Returns

Character string.

------------------------------------------------------------------------

### Method `get_params()`

Get layout parameters.

#### Usage

    CographLayout$get_params()

#### Returns

List of parameters.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print layout summary.

#### Usage

    CographLayout$print()

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
# Create a circular layout
layout <- CographLayout$new("circle")

# Apply to network
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- CographNetwork$new(adj)
coords <- layout$compute(net)
```
