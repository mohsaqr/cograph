# Convert Network to igraph Object

Converts various network representations to an igraph object. Supports
matrices, edge-list data frames, igraph objects, network objects,
cograph_network, and tna objects.

## Usage

``` r
to_igraph(x, directed = NULL)
```

## Arguments

- x:

  Network input. Can be:

  - A square numeric matrix (adjacency/weight matrix)

  - A data frame edge list with source and target columns

  - An igraph object (returned as-is or converted if directed differs)

  - A statnet network object

  - A cograph_network object

  - A tna object

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected.

## Value

An igraph object.

## See also

[`to_data_frame`](https://sonsoles.me/cograph/reference/to_data_frame.md),
[`as_cograph`](https://sonsoles.me/cograph/reference/as_cograph.md)

## Examples

``` r
to_igraph(regulation_net)
#> IGRAPH 3d28e61 DNW- 10 30 -- 
#> + attr: name (v/c), weight (e/n)
#> + edges from 3d28e61 (vertex names):
#>  [1] Explore   ->Reflect    Explore   ->Share      Plan      ->Monitor   
#>  [4] Plan      ->Discuss    Plan      ->Evaluate   Plan      ->Create    
#>  [7] Plan      ->Share      Monitor   ->Adapt      Monitor   ->Create    
#> [10] Adapt     ->Explore    Adapt     ->Discuss    Adapt     ->Synthesize
#> [13] Reflect   ->Explore    Reflect   ->Monitor    Discuss   ->Explore   
#> [16] Discuss   ->Reflect    Discuss   ->Create     Synthesize->Plan      
#> [19] Synthesize->Monitor    Synthesize->Reflect    Evaluate  ->Monitor   
#> [22] Evaluate  ->Adapt      Evaluate  ->Reflect    Create    ->Explore   
#> + ... omitted several edges
```
