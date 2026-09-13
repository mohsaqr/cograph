# Save as ggplot2

Save network as a ggplot2 object to file using ggsave.

## Usage

``` r
sn_save_ggplot(
  network,
  filename,
  width = 7,
  height = 7,
  dpi = 300,
  title = NULL,
  ...
)
```

## Arguments

- network:

  A cograph_network object.

- filename:

  Output filename. Format is detected from the extension by
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

- width:

  Width in inches (default 7).

- height:

  Height in inches (default 7).

- dpi:

  Resolution for raster formats (default 300).

- title:

  Optional plot title.

- ...:

  Additional arguments passed to ggsave.

## Value

The output `filename`, invisibly.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- cograph(adj)
sn_save_ggplot(net, file.path(tempdir(), "network.pdf"))
#> Saved to: /tmp/RtmpyiWXmJ/network.pdf
```
