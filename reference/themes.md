# Themes

A theme is a `CographTheme` object that sets the background, node, edge
and label colors of a plot. Themes are stored in a registry and selected
by name through the `theme` argument of
[`splot`](https://sonsoles.me/cograph/reference/splot.md),
[`soplot`](https://sonsoles.me/cograph/reference/soplot.md) and
[`sn_theme`](https://sonsoles.me/cograph/reference/sn_theme.md). The
functions `theme_cograph_*()` return the built-in themes:

- `theme_cograph_classic()`:

  Blue nodes and gray edges (`"classic"`).

- `theme_cograph_colorblind()`:

  Colors distinguishable under color vision deficiency (`"colorblind"`).

- `theme_cograph_gray()`:

  Black and white for print (`"gray"`, also `"grey"`).

- `theme_cograph_dark()`:

  Dark background for presentations (`"dark"`).

- `theme_cograph_minimal()`:

  Thin borders and few colors (`"minimal"`).

- `theme_cograph_viridis()`:

  The viridis palette (`"viridis"`).

- `theme_cograph_nature()`:

  Earth tones (`"nature"`).

`register_theme()` adds a theme under a new name, `get_theme()` returns
a registered theme, and `list_themes()` returns the names of all
registered themes.

## Usage

``` r
register_theme(name, theme)

get_theme(name)

list_themes()

theme_cograph_classic()

theme_cograph_colorblind()

theme_cograph_gray()

theme_cograph_dark()

theme_cograph_minimal()

theme_cograph_viridis()

theme_cograph_nature()
```

## Arguments

- name:

  Character. The name of the theme.

- theme:

  A `CographTheme` object, for example one created with
  `CographTheme$new()` or returned by a `theme_cograph_*()` function.

## Value

The `theme_cograph_*()` functions return a `CographTheme` object.
`register_theme()` returns `NULL` invisibly. `get_theme()` returns the
theme, or `NULL` if no theme has that name. `list_themes()` returns a
character vector of theme names.

## Examples

``` r
splot(regulation_net, theme = "dark")
```
