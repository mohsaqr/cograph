# Agent note — Nestimate and cograph meta\$splot contract

Audience: Nestimate agents and maintainers that create objects intended
to be rendered by
[`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.md).

**Goal.** Let Nestimate and other producer packages tell cograph how an
object should be rendered without adding a new
[`inherits()`](https://rdrr.io/r/base/class.html) branch in
[`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.md) for
every new class.

The contract lives on the object:

``` r

x$meta$splot <- list(
  renderer = "network",
  weight = NULL,
  defaults = list()
)
```

cograph reads this metadata at plot time. It is a rendering hint, not
analytical state. The object should still carry its real model fields
(`weights`, `edges`, `nodes`, bootstrap fields, permutation fields,
etc.) exactly as before.

------------------------------------------------------------------------

## Precedence

Always preserve this rule:

``` r
user arguments > x$meta$splot$defaults > cograph defaults
```

Example:

``` r

net$meta$splot <- list(
  renderer = "network",
  defaults = list(layout = "oval", node_fill = "white")
)

cograph::splot(net)                       # uses oval + white
cograph::splot(net, layout = "spring")    # user layout wins
cograph::splot(net, node_fill = "grey80") # user fill wins
```

Do not make metadata defaults sticky, locked, or impossible to override.

------------------------------------------------------------------------

## Fields

### `renderer`

Character scalar. `"network"` means the normal
[`splot()`](https://sonsoles.me/cograph/reference/splot.md) network
renderer. Aliases: `"splot"`, `"base"`, `"default"`.

Specialized renderers are resolved through cograph’s internal whitelist
only. Do not store arbitrary function names. Current intended names
include:

- `"difference"` / `"compare"` -\>
  [`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md)
- `"bootstrap"` / `"net_bootstrap"` -\>
  [`splot.net_bootstrap()`](https://sonsoles.me/cograph/reference/splot.md)
- `"tna_bootstrap"` -\>
  [`splot.tna_bootstrap()`](https://sonsoles.me/cograph/reference/splot.tna_bootstrap.md)
- `"permutation"` / `"net_permutation"` -\>
  [`splot.net_permutation()`](https://sonsoles.me/cograph/reference/splot.md)
- `"tna_permutation"` -\>
  [`splot.tna_permutation()`](https://sonsoles.me/cograph/reference/plot_permutation.md)
- `"stability"` / `"net_stability"` -\>
  [`plot_net_stability()`](https://sonsoles.me/cograph/reference/plot_net_stability.md)
- `"mlvar"` / `"net_mlvar"` -\>
  [`splot.net_mlvar()`](https://sonsoles.me/cograph/reference/splot.md)
- `"netobject"` -\>
  [`splot.netobject()`](https://sonsoles.me/cograph/reference/splot.md)
- `"netobject_group"` -\>
  [`plot_netobject_group()`](https://sonsoles.me/cograph/reference/plot_netobject_group.md)
- `"netobject_ml"` -\>
  [`plot_netobject_ml()`](https://sonsoles.me/cograph/reference/plot_netobject_ml.md)
- `"net_bootstrap_group"` -\>
  [`plot_net_bootstrap_group()`](https://sonsoles.me/cograph/reference/plot_net_bootstrap_group.md)
- `"boot_glasso"` -\>
  [`splot.boot_glasso()`](https://sonsoles.me/cograph/reference/splot.md)
- `"wtna_mixed"` -\>
  [`splot.wtna_mixed()`](https://sonsoles.me/cograph/reference/splot.md)

The object must still match the renderer’s expected fields. For example,
`renderer = "bootstrap"` does not turn an arbitrary object into a
bootstrap; it only routes to
[`splot.net_bootstrap()`](https://sonsoles.me/cograph/reference/splot.md).

### `weight`

Optional character scalar naming the default edge quantity to render.

Use this when an object stores multiple edge quantities and one should
be the default plot view.

``` r

fit$meta$splot <- list(
  renderer = "network",
  weight = "adj_res"
)
```

cograph will look for:

- `fit$adj_res`: if it is a matrix, it becomes the rendered network —
  copied to `fit$weights` and the drawn edge set is rebuilt from its
  nonzero cells (aligned to the object’s node order via dimnames when
  present). Use this form when the alternate quantity can be nonzero
  where the original edge list has no edge (e.g. a residual at a
  zero-count transition).
- `fit$edges$adj_res`: copied to `fit$edges$weight` on the plotting
  copy; the producer’s edge set is kept and `fit$weights` is rebuilt to
  stay consistent.

If both exist, the matrix wins. Do not duplicate large matrices inside
`meta$splot`. Store the name of an existing edge column or matrix.

### `defaults`

Named list of arguments to pass as defaults to the selected renderer.

For normal network plots:

``` r

defaults = list(
  layout = "oval",
  node_fill = "white",
  edge_labels = TRUE,
  weight_digits = 1
)
```

For bootstrap-like views:

``` r

defaults = list(
  display = "styled",
  show_ci = TRUE
)
```

For permutation-like views:

``` r

defaults = list(
  show_nonsig = FALSE,
  show_stars = TRUE
)
```

------------------------------------------------------------------------

## Nestimate guidance

Nestimate already returns many objects that cograph knows how to plot by
class. Keep those paths working. `meta$splot` is for making new
Nestimate objects self-describing and for reducing the need to add
future cograph-side branches.

### Plain `netobject`

Use the normal network renderer:

``` r

net$meta$splot <- list(
  renderer = "network",
  defaults = list(
    labels = net$nodes$label,
    tna_styling = TRUE,
    donut_fill = net$initial
  )
)
```

Only include defaults that are present. If `net$initial` is absent, do
not add a `donut_fill = NULL` entry unless disabling inherited rings is
the explicit intent.

### Difference-like objects

If the object already carries the display difference matrix in
`$weights`, use:

``` r

diff$meta$splot <- list(
  renderer = "difference",
  defaults = list(minimum = 0)
)
```

The object must still carry the fields that
[`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md)
expects (`weights`, `difference_matrix`, or equivalent current
contract).

### Bootstrap-like objects

If the object follows the `net_bootstrap` field contract:

``` r

boot$meta$splot <- list(
  renderer = "bootstrap",
  defaults = list(display = "styled")
)
```

Required shape remains the same as
[`splot.net_bootstrap()`](https://sonsoles.me/cograph/reference/splot.md)
expects: original network weights, original nodes/directedness, p-values
when styled significance is desired, and CI fields when CI
underlays/labels are requested.

### Permutation-like objects

If the object follows the `net_permutation` field contract:

``` r

perm$meta$splot <- list(
  renderer = "permutation",
  defaults = list(show_nonsig = FALSE, show_stars = TRUE)
)
```

The object must still provide `diff`, `diff_sig`, `p_values`, and
source-network metadata as expected by
[`splot.net_permutation()`](https://sonsoles.me/cograph/reference/splot.md).

------------------------------------------------------------------------

## lagdynamics example

`lagdynamics::lsa()` already returns
`class = c("lsa", "cograph_network")` and stores several edge
quantities: counts, probabilities, adjusted residuals, Yule’s Q, lift,
etc. Its default network view is residual-based, not count-based.

The object-side hint should therefore be:

``` r

fit$meta$splot <- list(
  renderer = "network",
  weight = "adj_res",
  defaults = list(
    node_fill = "white",
    edge_labels = TRUE,
    weight_digits = 1,
    edge_positive_color = "#4A6FA5",
    edge_negative_color = "#B04A4A",
    node_border_color = "steelblue",
    node_border_width = 1.1
  )
)
```

[`plot_transitions()`](https://sonsoles.me/cograph/reference/plot_transitions.md)
can remain the explicit view selector for `weights = "prob"`, `"count"`,
`"lift"`, `"yules_q"`, significance filtering, and top-edge pruning. The
default `splot(fit)` view can be driven by the metadata.

------------------------------------------------------------------------

## Do not store

Do not put these in `meta$splot`:

- recorded plots or graphics devices
- rendered coordinates unless using the existing `meta$layout` contract
- duplicated model results
- raw data
- arbitrary function names
- package imports or closures

Store only small strings and default arguments.
