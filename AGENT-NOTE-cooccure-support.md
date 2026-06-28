# Agent note — add native cooccure support to cograph

**Goal.** Let cograph plot `cooccure` outputs **directly**, with no
converter step on the user’s side:

``` r

res  <- cooccure::cooccurrence(data, field = "Authors", sep = ";")
boot <- cooccure::co_bootstrap(res, engine = "bayes")
cograph::splot(res)    # currently errors / needs cooccure::as_cograph(res)
cograph::splot(boot)   # should plot the credible (stable) network
```

**Why this belongs in cograph (not cooccure).** cograph is the rendering
hub and already hosts `splot` methods for *other* packages’ classes —
`splot.netobject`, `splot.net_bootstrap`, `splot.net_permutation`,
`splot.boot_glasso`, `splot.net_mlvar`, `splot.tna_bootstrap`, … (see
`R/plot-nestimate.R`, `R/plot-bootstrap.R`). Adding `splot.cooccurrence`
/ `splot.co_bootstrap` is the same pattern. This matches the Dynalytics
compute-vs-render contract: producers emit data, cograph dispatches on
class.

Today the bridge lives on the **cooccure** side
(`as_cograph.cooccurrence`, `as_cograph.co_bootstrap` in
`cooccure/R/converters.R`). Those are the reference implementation — the
native cograph methods should inline the same construction.

------------------------------------------------------------------------

## What to implement

Add a new file `R/plot-cooccure.R` with two S3 methods registered on the
existing `splot` generic (`R/splot.R:372`):

- `splot.cooccurrence(x, ...)`
- `splot.co_bootstrap(x, display = c("stable", "full"), weight = c("boot_mean", "weight"), ...)`

Both build a symmetric weight matrix + node/edge structure and hand off
to the core renderer the way `splot.netobject` / `splot.net_bootstrap`
do.

### Dependency direction — important

**Do NOT `Import` cooccure** (cooccure already `Suggests` cograph;
importing back would risk a cycle). Duck-type instead: dispatch on the
S3 class and read attributes/columns by name. cograph never needs to
load cooccure to plot its objects. Keep cooccure out of `DESCRIPTION`
(or at most `Suggests` for tests).

------------------------------------------------------------------------

## The cooccure object contract (read these, don’t guess)

### Estimator result — class `c("cooccurrence", "data.frame")`

- **Columns:** `from`, `to`, `weight`, `count` (+ `group` when
  `split_by` used).
- **Attributes:** `matrix` (normalized, a sparse `Matrix`), `raw_matrix`
  (counts), `items` (character node vector — the full node set),
  `frequencies`, `similarity`, `scale`, `threshold`, `min_occur`,
  `n_transactions`, `n_items`.
- **Build the graph from the `matrix` attribute** (densify with
  [`as.matrix()`](https://rdrr.io/r/base/matrix.html) if it
  `inherits(., "Matrix")`); fall back to the `from`/`to`/`weight` edge
  list only if the attribute is absent. Nodes = `attr(x, "items")`.

### Bootstrap result — class `c("co_bootstrap", "cooccurrence", "data.frame")`

- **Columns:** `from`, `to`, `weight` (observed), `boot_mean`,
  `boot_se`, `ci_low`, `ci_high`, `cr_lower`, `cr_upper`, `prop_within`,
  **`stable`** (logical: two-sided consistency-range flag, the
  credible-edge indicator).
- **Attributes:** `items`, `similarity`, `counting`, `n_transactions`,
  `n_items`, `R`, `engine` (`"classic"`/`"bayes"`), `ci`,
  `consistency_range`, `consistency`.
- **Caveat:** the bootstrap object does **not** carry `matrix` /
  `raw_matrix` / `frequencies`. So `splot.co_bootstrap` must build the
  matrix from the **edge list**: `from`/`to` + the chosen `weight`
  column.
- **Default view = the credible network:** keep only rows with
  `stable == TRUE` and weight edges by `boot_mean` (so the drawn network
  is bootstrap-derived). `display = "full"` keeps all edges;
  `weight = "weight"` uses observed weights. This mirrors
  `splot.net_bootstrap`’s `display` argument (`R/plot-bootstrap.R:344`).

### Matrix construction (mirror `cooccure::as_cograph.*`)

    items <- sort(unique(c(edges$from, edges$to)))   # or attr(x,"items") for the full set
    k <- length(items); M <- matrix(0, k, k, dimnames = list(items, items))
    i <- match(edges$from, items); j <- match(edges$to, items)
    M[cbind(i,j)] <- vals; M[cbind(j,i)] <- vals      # undirected/symmetric

For `co_bootstrap`, `edges` is the `stable`-filtered frame and `vals` is
`boot_mean`. Drop isolated nodes for a legible plot (use the items that
appear in the kept edges), unless a `keep_isolates` arg is added.

------------------------------------------------------------------------

## Suggested method skeleton

``` r

#' @export
splot.cooccurrence <- function(x, ...) {
  mat <- .cooccure_matrix(x)          # from `matrix` attr, else edge list
  splot(.as_cograph_network(mat), ...) # reuse cograph's cograph_network path
}

#' @export
splot.co_bootstrap <- function(x, display = c("stable", "full"),
                               weight = c("boot_mean", "weight"), ...) {
  display <- match.arg(display); weight <- match.arg(weight)
  edges <- as.data.frame(x)
  if (display == "stable") edges <- edges[!is.na(edges$stable) & edges$stable, ]
  mat <- .cooccure_edges_to_matrix(edges, value = weight)
  splot(.as_cograph_network(mat), ...)
}
```

`.as_cograph_network(mat)` = the same `cograph_network` list cograph
already plots (`weights`, `nodes`, `edges`, `directed = FALSE`,
`n_nodes`, `n_edges`, `meta`). If cograph has an internal
matrix→`cograph_network` helper, reuse it; otherwise copy the structure
from `cooccure::as_cograph.cooccurrence`.

Optionally also add `soplot.cooccurrence` / `soplot.co_bootstrap` (the
grid/ggplot backend) for parity, delegating the same way.

------------------------------------------------------------------------

## Tests (cograph side)

- `splot(cooccurrence_result)` and `splot(co_bootstrap_result)` render
  without error (skip_if_not_installed(“cooccure”), or use a hand-built
  object carrying the documented class/columns/attributes so no real dep
  is needed).
- `display = "stable"` plots only `stable` edges; `"full"` plots all.
- `weight = "weight"` vs `"boot_mean"` change edge widths as expected.
- Duck-typed: passing a bare data.frame with the right class + columns
  plots, proving cograph never calls into cooccure.

## When done

Once `splot.cooccurrence` / `splot.co_bootstrap` ship in cograph, the
`as_cograph.*` converters in cooccure become optional convenience (keep
them for explicit matrix/netobject extraction), and users can call
[`splot()`](https://sonsoles.me/cograph/reference/splot.md) directly on
any cooccure result. Note the parallel: cooccure’s own
`as_cograph.co_bootstrap` defaults to the stable subgraph + `boot_mean`
— keep the cograph method’s defaults identical so both paths draw the
same network. \`\`\`
