# CographTheme R6 Class

Class for managing visual themes for network plots.

## Value

A `CographTheme` R6 object.

## Active bindings

- `name`:

  Theme name.

## Methods

### Public methods

- [`CographTheme$new()`](#method-CographTheme-new)

- [`CographTheme$get()`](#method-CographTheme-get)

- [`CographTheme$set()`](#method-CographTheme-set)

- [`CographTheme$get_all()`](#method-CographTheme-get_all)

- [`CographTheme$merge()`](#method-CographTheme-merge)

- [`CographTheme$clone_theme()`](#method-CographTheme-clone_theme)

- [`CographTheme$print()`](#method-CographTheme-print)

- [`CographTheme$clone()`](#method-CographTheme-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new CographTheme object.

#### Usage

    CographTheme$new(
      name = "custom",
      background = "white",
      node_fill = "#4A90D9",
      node_border = "#2C5AA0",
      node_border_width = 1,
      edge_color = "gray50",
      edge_positive_color = "#2E7D32",
      edge_negative_color = "#C62828",
      edge_width = 1,
      label_color = "black",
      label_size = 10,
      title_color = "black",
      title_size = 14,
      legend_background = "white"
    )

#### Arguments

- `name`:

  Theme name (optional).

- `background`:

  Background color.

- `node_fill`:

  Default node fill color.

- `node_border`:

  Default node border color.

- `node_border_width`:

  Default node border width.

- `edge_color`:

  Default edge color.

- `edge_positive_color`:

  Color for positive edge weights.

- `edge_negative_color`:

  Color for negative edge weights.

- `edge_width`:

  Default edge width.

- `label_color`:

  Default label color.

- `label_size`:

  Default label size.

- `title_color`:

  Title color.

- `title_size`:

  Title size.

- `legend_background`:

  Legend background color.

#### Returns

A new CographTheme object.

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get a theme parameter.

#### Usage

    CographTheme$get(name)

#### Arguments

- `name`:

  Parameter name.

#### Returns

Parameter value.

------------------------------------------------------------------------

### Method `set()`

Set a theme parameter.

#### Usage

    CographTheme$set(name, value)

#### Arguments

- `name`:

  Parameter name.

- `value`:

  Parameter value.

------------------------------------------------------------------------

### Method `get_all()`

Get all theme parameters.

#### Usage

    CographTheme$get_all()

#### Returns

List of parameters.

------------------------------------------------------------------------

### Method [`merge()`](https://rdrr.io/r/base/merge.html)

Merge with another theme.

#### Usage

    CographTheme$merge(other)

#### Arguments

- `other`:

  Another CographTheme or list of parameters.

#### Returns

A new merged CographTheme.

------------------------------------------------------------------------

### Method `clone_theme()`

Clone the theme.

#### Usage

    CographTheme$clone_theme()

#### Returns

A new CographTheme.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print theme summary.

#### Usage

    CographTheme$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CographTheme$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Create a custom theme
theme <- CographTheme$new(
  background = "white",
  node_fill = "steelblue",
  edge_color = "gray60"
)
```
