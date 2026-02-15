# Project Learnings

### 2026-02-15
- [mcml refactor]: Separated data extraction from plotting. `mcml()` is now a pure data extraction function that returns a `cograph_mcml` object. `plot_mcml()` uses `mcml()` internally and returns the data invisibly. This follows cograph's established pattern: extraction -> transformation -> visualization.
- [plot_mcml]: Added 22 new parameters for full visualization control. Summary labels now shown by default (`summary_labels = TRUE`). Arrows shown by default on summary edges (`summary_arrows = TRUE`). All hardcoded values (edge widths, alphas, layout multipliers) now parameterized.
- [cluster_summary alignment]: Aligned `cluster_summary()` with `mcml()` structure. Now includes `tna` (row-normalized transition matrix), `inits` (initial state distribution), and `as_tna` parameter. **Breaking change**: renamed `between` → `between_weights` and `within` → `within_weights` for consistency with mcml. Both functions now return identical `tna` and `inits` fields.
- [within_tna]: Added `within_tna` (n×n block-diagonal matrix) and `within_inits` to `cluster_summary()`. The within_tna shows row-normalized transitions between nodes within the same cluster. Each diagonal block corresponds to one cluster's internal transition dynamics.

### 2026-02-14
- [label resolution]: Node labels now use priority order: `labels` > `label` > identifier. The `labels` column takes precedence when both exist. Updated in `get_labels()`, `resolve_labels()`, `render_node_labels_grid()`, and `CographNetwork.node_labels`.
- [cograph support]: `cluster_summary()` now accepts cograph_network and tna objects directly, extracting the weight matrix automatically. Uses `x$weights` for cograph_network (efficient) or `to_matrix()` as fallback.
- [testing]: CographNetwork R6 class does not have a `node_labels` parameter in `$new()` - use the `nodes` parameter with a data frame instead. Tests were incorrectly written and needed fixing.
