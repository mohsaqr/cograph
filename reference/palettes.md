# Color Palettes

The `palette_*()` functions generate a vector of `n` colors for nodes or
edges. They are registered under their short names (for example
`"colorblind"`), which
[`sn_palette`](https://sonsoles.me/cograph/reference/sn_palette.md)
accepts, and `list_palettes()` returns the registered names.

- `palette_rainbow()`:

  Rainbow hues.

- `palette_colorblind()`:

  The colorblind-safe colors of Wong.

- `palette_pastel()`:

  Soft pastel colors.

- `palette_viridis()`:

  The viridis family, chosen by `option`.

- `palette_blues()`, `palette_reds()`:

  Sequential blue or red shades.

- `palette_diverging()`:

  Blue to red through `midpoint`.

## Usage

``` r
list_palettes()

palette_rainbow(n, alpha = 1)

palette_colorblind(n, alpha = 1)

palette_pastel(n, alpha = 1)

palette_viridis(n, alpha = 1, option = "viridis")

palette_blues(n, alpha = 1)

palette_reds(n, alpha = 1)

palette_diverging(n, alpha = 1, midpoint = "white")
```

## Arguments

- n:

  Number of colors to generate.

- alpha:

  Transparency, from 0 (transparent) to 1 (opaque).

- option:

  Viridis option, one of `"viridis"`, `"magma"`, `"plasma"`,
  `"inferno"`, `"cividis"`. Any other value gives the `"viridis"`
  colors.

- midpoint:

  Color of the midpoint of the diverging palette.

## Value

The `palette_*()` functions return a character vector of `n` colors.
`list_palettes()` returns a character vector of palette names.

## Examples

``` r
splot(regulation_net, node_fill = palette_colorblind(n = 10))
```
