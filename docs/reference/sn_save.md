# Save Network Visualization

Save a Cograph network visualization to a file.

## Usage

``` r
sn_save(network, filename, width = 7, height = 7, dpi = 300, title = NULL, ...)
```

## Arguments

- network:

  A cograph_network object, matrix, data.frame, or igraph object.
  Matrices and other inputs are auto-converted.

- filename:

  Output filename. Format is detected from extension.

- width:

  Width in inches (default 7).

- height:

  Height in inches (default 7).

- dpi:

  Resolution for raster formats (default 300).

- title:

  Optional plot title.

- ...:

  Additional arguments passed to the graphics device.

## Value

The output `filename`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- cograph(adj)
sn_save(net, file.path(tempdir(), "network.pdf"))
} # }
```
