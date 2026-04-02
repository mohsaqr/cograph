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

  Output filename.

- width:

  Width in inches.

- height:

  Height in inches.

- dpi:

  Resolution for raster formats.

- title:

  Optional plot title.

- ...:

  Additional arguments passed to ggsave.

## Value

The output `filename`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- cograph(adj)
sn_save_ggplot(net, file.path(tempdir(), "network.pdf"))
} # }
```
