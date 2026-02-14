# Project Learnings

### 2026-02-14
- [label resolution]: Node labels now use priority order: `labels` > `label` > identifier. The `labels` column takes precedence when both exist. Updated in `get_labels()`, `resolve_labels()`, `render_node_labels_grid()`, and `CographNetwork.node_labels`.
- [cograph support]: `cluster_summary()` now accepts cograph_network and tna objects directly, extracting the weight matrix automatically. Uses `x$weights` for cograph_network (efficient) or `to_matrix()` as fallback.
- [testing]: CographNetwork R6 class does not have a `node_labels` parameter in `$new()` - use the `nodes` parameter with a data frame instead. Tests were incorrectly written and needed fixing.
