# Agent note — plot_htna() / plot_mtna() single-community error message

Audience: agents about to modify `R/plot-htna.R` (`plot_htna`) or
`R/plot-htna-multi.R` (`plot_mtna`).

**Status: KNOWN, deliberately NOT fixed (2026-07-04).** Documented here
so a future change in these files can address it in-context rather than
rediscover it. This is a message-quality issue, not a broken feature —
do not treat it as a regression.

## The finding

`plot_htna(x, community = <method>)` and
`plot_mtna(x, community = <method>)` run
`detect_communities(x, method = community)` internally, then
[`split()`](https://rdrr.io/r/base/split.html) the result into
`node_list` / `cluster_list`. When the chosen algorithm returns a
**single community**, that split yields a length-1 list and execution
hits the generic guard:

``` r

# R/plot-htna.R ~line 242
stop("node_list must be a list of 2+ character vectors", call. = FALSE)
# R/plot-htna-multi.R ~line 193
stop("cluster_list must be a list of 2+ character vectors", call. = FALSE)
```

The message names `node_list` / `cluster_list` — an argument the caller
never supplied (they passed `community =`). Confusing, but the *refusal
itself is correct*: a hierarchical / multi-cluster layout genuinely
needs 2+ groups.

## When it triggers

Community methods that run on the **directed** graph and can collapse a
sparse transition network to one community: `walktrap`, `infomap`,
`label_prop`. On a real `tna(group_regulation[1:200,])` these return
**1** community, so `plot_htna(t, community = "walktrap")` errors with
the misleading text.

Methods that collapse to undirected first — `louvain`, `leiden` (via the
`detect_communities` directed-graph fix, see NEWS 2.4.4), and
`fast_greedy` (always) — return 4 / 9 / 4 communities on the same object
and render fine.

## Suggested fix (for when these files are next touched)

Right after the `detect_communities` call in each function, before the
generic guard, add a count check that names the actual cause:

``` r

if (length(node_list) < 2) {           # cluster_list in plot_mtna
  stop("community = \"", community, "\" found only ", length(node_list),
       " community, but plot_htna() needs at least 2 groups. Try another ",
       "method (e.g. \"louvain\") or pass node_list explicitly.",
       call. = FALSE)
}
```

Keep the existing `node_list`/`cluster_list` guard intact — a genuine
single-group `node_list = list(All = ...)` misuse should still get that
message. Add deterministic regression tests that **stub**
`detect_communities` to return `data.frame(node = ..., community = 1L)`
rather than relying on a specific algorithm’s output on a specific
matrix (a 4-node cycle gives walktrap *2* communities, so a hand-built
fixture is fragile — verified 2026-07-04).

## Do NOT

- Do not relax the 2+ requirement — the layouts need at least two
  groups.
- Do not change `detect_communities` for this; the count guard belongs
  in the plot functions, which own the 2+ contract.
