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

  Output filename. Format is detected from the extension; one of `.pdf`,
  `.png`, `.svg`, `.jpeg`/`.jpg`, `.tiff`, `.eps`/`.ps`.

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
sn_save(cograph(regulation_net),
  filename = file.path(tempdir(), "network.pdf"))
#> Saved to: /tmp/RtmpamgBFq/network.pdf
```
