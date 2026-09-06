#!/usr/bin/env python3
"""Regenerate docs/CENTRALITY-ZOO-COVERAGE.md from the Centrality Zoo matrix.

Source: https://centralityzoo.github.io/comparison/ (Shvydun 2025),
data file /assets/data/correlation.json — average Kendall rank correlation
between 349 node measures over 648 ICON networks. A minified gzipped copy
(correlation.json.gz) is kept beside this script. Run from the repository root:

    python3 docs/zoo/zoo_coverage.py
"""
import gzip
import json
import statistics
from pathlib import Path

HERE = Path(__file__).resolve().parent
OUT = HERE.parent / "CENTRALITY-ZOO-COVERAGE.md"

# Zoo label -> cograph measure name(s) in centrality(). Hand-curated.
COVERED = {
    "Degree": "degree", "Closeness": "closeness", "Eccentricity": "eccentricity",
    "k-shell": "coreness", "Harmonic": "harmonic", "Diffusion Degree": "diffusion",
    "Leverage": "leverage", "m-reach": "kreach", "Katz": "katz (also alpha)",
    "Lin's index": "lin", "Decay": "decay", "Residual closeness": "residual_closeness",
    "Lobby index": "lobby", "Entropy": "entropy",
    "Semi-local ranking (SLC)": "semilocal", "ClusterRank": "clusterrank",
    "BottleNeck": "bottleneck", "Centroid": "centroid", "MNC": "mnc", "DMNC": "dmnc",
    "Closeness vitality": "closeness_vitality", "Integration": "integration",
    "Expected force (ExF)": "expected", "Gil-Schmidt Power Index": "gilschmidt",
    "Participation coefficient": "participation",
    "Intra-module degree": "within_module_z", "Gateway coefficient": "gateway",
    "Gravity centrality": "gravity", "CollInf": "collective_influence",
    "Local H-index": "local_hindex", "h-index strength": "hindex_strength",
    "Betweenness": "betweenness", "Eigenvector": "eigenvector", "PageRank": "pagerank",
    "Burt's constraint": "constraint",
    "Local clustering coefficient": "transitivity", "Subgraph": "subgraph",
    "Laplacian": "laplacian", "Load": "load",
    "Current-flow Closeness": "current_flow_closeness (also information)",
    "Current-flow betweenness": "current_flow_betweenness", "VoteRank": "voterank",
    "Percolation": "percolation", "Stress": "stress",
    "Flow betweenness": "flow_betweenness", "Total communicability": "communicability",
    "Communicability betweenness": "communicability_betweenness",
    "Random walk centrality": "random_walk", "Topological": "topological_coefficient",
    "Bridging centrality": "bridging",
    "Localized bridging centrality": "local_bridging",
    "Effective size": "effective_size", "Borgatti's effective size": "effective_size",
    "Diversity coefficient": "diversity", "Cross-Clique Connectivity": "cross_clique",
    "Markov": "markov", "SALSA": "salsa", "LeaderRank": "leaderrank",
    "Second order centrality": "second_order", "Infection number": "infection",
    "Non-backtracking centrality": "nonbacktracking",
    "Spanning tree centrality (STC)": "spanning_tree", "Hubbel": "hubbell",
    "Pairwise disconnectivity": "pairwisedis",
    # Batch 7 (2026-09-06)
    "Distance entropy": "distance_entropy [batch 7]",
    "Local dimension (Pu)": "local_dimension [batch 7]",
    "Local information dimensionality (LID)": "local_information_dimension [batch 7]",
    "Modularity vitality": "modularity_vitality [batch 7]",
    "Neighborhood connectivity": "neighborhood_connectivity [batch 7]",
    # Batch 8 (2026-09-06)
    "Shapley value (game 1)": "shapley_game1 [batch 8]",
    "Shapley value (game 2)": "shapley_game2 [batch 8]",
    "Shapley value (game 3)": "shapley_game3 [batch 8]",
    "Access information": "access_information [batch 8]",
    "Hide information": "hide_information [batch 8]",
    "Rumor centrality": "rumor [batch 8]",
    "Community Hub\u2011Bridge measure": "community_hub_bridge [batch 8]",
    "Entropy variation (degree)": "entropy_variation_degree [batch 8]",
    "Entropy variation (betweenness)": "entropy_variation_betweenness [batch 8]",
    "s-shell index": "s_shell [batch 8]",
    "DegreeDiscountIC": "degree_discount [batch 8]",
    "SingleDiscount": "single_discount [batch 8]",
    "NCVoteRank": "ncvoterank [batch 8]",
}

# cograph measures with no Zoo counterpart (for completeness).
COGRAPH_ONLY = [
    "strength", "authority", "hub", "power", "radiality", "dangalchev",
    "generalized_closeness", "harary", "average_distance", "barycenter",
    "wiener", "lac", "onion", "reaching_local", "trophic_level",
    "prestige_domain", "prestige_domain_proximity",
    "brokerage_coordinator", "brokerage_itinerant", "brokerage_representative",
    "brokerage_gatekeeper", "brokerage_liaison",
    "expected_influence_1", "expected_influence_2",
]

# Ranked backlog from the 2026-09-06 review (max tau vs existing measures).
PLANNED = []
SKIPPED = [
    ("Hybrid (Pozzi)", "tau 0.00 with all 348 partners: looks like a broken run, not a measure"),
    ("Physarum / K-shell Physarum centrality", "iterative flow solver, heavy"),
    ("Absorbing Random-Walk (ARW)", "set-function greedy, heavy"),
    ("LRIC family, SRIC, Interdependence", "author's own long-range-influence measures; no closed form"),
    ("Node information dimension (NID)", "box scheme in the encyclopedia is ambiguous; original paper paywalled"),
    ("Game centrality (GC), Algebraic centrality, Community centrality", "definition not pinned"),
]


LOOKUP = HERE.parent / "CENTRALITY-ZOO-LOOKUP.md"
ARTICLE = HERE.parent.parent / "vignettes" / "articles" / "centrality-zoo-lookup.Rmd"
ARTICLE_HEADER = """---
title: "Centrality Zoo lookup"
output: rmarkdown::html_vignette
vignette: >
  %\\VignetteIndexEntry{Centrality Zoo lookup}
  %\\VignetteEngine{knitr::rmarkdown}
  %\\VignetteEncoding{UTF-8}
---

The [Centrality Zoo](https://centralityzoo.github.io/) (Shvydun, 2025,
*Zoo of Centralities: Encyclopedia of Node Metrics in Complex Networks*,
[arXiv:2511.05122](https://arxiv.org/abs/2511.05122)) catalogues 349 node
centrality measures and publishes how similarly they rank nodes. This page
answers one question: **if you are looking for a measure from the Zoo, what
does `cograph` offer?**

"""


ON_THE_WAY = []


def clean_name(measure):
    """Strip the annotations used in the coverage tables."""
    return measure.split(" ")[0]


def call_for(measure):
    return f'`centrality(x, measures = "{clean_name(measure)}")`'


def write_lookup(labels, idx, M, cov, rows):
    """Reader-facing lookup: what cograph has for each Zoo measure."""
    missing = [l for l in ON_THE_WAY if l not in idx]
    if missing:
        raise SystemExit(f"ON_THE_WAY labels not in zoo: {missing}")
    nearest = {l: (t, c) for t, l, c in rows}
    by_name = lambda l: l.lower()
    identical = sorted((l for l in nearest if nearest[l][0] >= 0.99), key=by_name)
    near = sorted((l for l in nearest if 0.90 <= nearest[l][0] < 0.99), key=by_name)
    planned = [l for l in sorted(ON_THE_WAY, key=by_name) if l in nearest and nearest[l][0] < 0.90]
    absent = sorted((l for l in nearest if nearest[l][0] < 0.90 and l not in ON_THE_WAY),
                    key=by_name)

    L = []
    w = L.append
    w("Every measure in the [Centrality Zoo](https://centralityzoo.github.io/comparison/)")
    w(f"({len(labels)} measures, Shvydun 2025) appears exactly once below. `tau` is the")
    w("average Kendall rank correlation between the Zoo measure and the named")
    w("cograph measure over 648 real networks: how often the two agree on which of")
    w("two nodes ranks higher. 1.00 means always.")
    w("")
    w("| Section | Meaning | Count |")
    w("|---|---|---|")
    w(f"| Available | cograph implements the measure itself | {len(cov)} |")
    w(f"| Almost identical (tau >= 0.99) | same node ranking on essentially every network; use the cograph measure | {len(identical)} |")
    w(f"| Near-duplicate (0.90 <= tau < 0.99) | nearly the same ranking, a few nodes swap places; a workable substitute | {len(near)} |")
    w(f"| On the way | planned for a later batch | {len(planned)} |")
    w(f"| Not available | cograph has nothing comparable (best tau below 0.90) | {len(absent)} |")
    w("")
    w("Caveat: the Zoo computed tau on undirected, unweighted networks, so")
    w("agreement on a directed or weighted network may be lower. Rank agreement")
    w("says nothing about the raw values, which differ in scale.")
    w("")
    w("## Available in cograph")
    w("")
    w("| Zoo measure | cograph measure | Call |")
    w("|---|---|---|")
    for l in sorted(cov, key=by_name):
        m = clean_name(COVERED[l])
        w(f"| {l} | `{m}` | {call_for(m)} |")
    w("")
    w("## Almost identical to a cograph measure (tau >= 0.99)")
    w("")
    w("The Zoo measure and the cograph measure rank nodes the same way on")
    w("virtually every network. Use the cograph measure.")
    w("")
    w("| Zoo measure | Use instead | Call | tau |")
    w("|---|---|---|---|")
    for l in identical:
        t, c = nearest[l]; m = clean_name(COVERED[c])
        w(f"| {l} | `{m}` | {call_for(m)} | {t:.2f} |")
    w("")
    w("## Near-duplicate of a cograph measure (0.90 <= tau < 0.99)")
    w("")
    w("Nearly the same ranking; differences are confined to a handful of nodes.")
    w("")
    w("| Zoo measure | Closest cograph measure | Call | tau |")
    w("|---|---|---|---|")
    for l in near:
        t, c = nearest[l]; m = clean_name(COVERED[c])
        w(f"| {l} | `{m}` | {call_for(m)} | {t:.2f} |")
    w("")
    w("## On the way")
    w("")
    if planned:
        w("Planned for a later release. Nothing in cograph substitutes for them yet.")
        w("")
        for l in planned:
            w(f"- {l}")
    else:
        w("None at present.")
    w("")
    w("## Not available")
    w("")
    w("cograph has no measure that ranks nodes like these (best tau below 0.90).")
    w("")
    for l in absent:
        w(f"- {l}")
    w("")
    body = "\n".join(L)
    LOOKUP.write_text(
        "# Looking for a centrality from the Zoo? Here is what cograph has\n\n"
        + body
        + "Generated by `docs/zoo/zoo_coverage.py`; see `CENTRALITY-ZOO-COVERAGE.md`\n"
        + "for the same data with nearest-measure detail and the ranked backlog.\n")
    ARTICLE.write_text(ARTICLE_HEADER + body)

def main():
    with gzip.open(HERE / "correlation.json.gz", "rt", encoding="utf-8") as fh:
        d = json.load(fh)
    labels, M = d["labels"], d["matrix"]
    idx = {l: i for i, l in enumerate(labels)}
    missing = [l for l in COVERED if l not in idx]
    if missing:
        raise SystemExit(f"labels not in zoo: {missing}")
    cov = [l for l in labels if l in COVERED]
    unc = [l for l in labels if l not in COVERED]

    rows = []
    for l in unc:
        i = idx[l]
        best = max((M[i][idx[c]], c) for c in cov)
        rows.append((best[0], l, best[1]))
    rows.sort(key=lambda r: (-r[0], r[1]))

    def bucket(lo, hi):
        return [r for r in rows if lo <= r[0] < hi]
    identical = bucket(0.99, 1.01)
    near = bucket(0.90, 0.99)
    some = bucket(0.70, 0.90)
    novel = bucket(-1, 0.70)

    off = [M[i][j] for i in range(len(labels)) for j in range(i)]

    L = []
    w = L.append
    w("# Centrality Zoo coverage")
    w("")
    w("What the [Centrality Zoo comparison](https://centralityzoo.github.io/comparison/)")
    w("(Shvydun 2025, *Zoo of Centralities*, arXiv:2511.05122) says about cograph's")
    w("`centrality()` surface. Generated by `docs/zoo/zoo_coverage.py` from the")
    w("site's `correlation.json` (gzipped copy in `docs/zoo/`). Regenerate after adding")
    w("measures; edit the `COVERED` map in the script, not this file.")
    w("")
    w("## Source")
    w("")
    w("- Matrix of average **Kendall rank correlation** between node rankings,")
    w(f"  {len(labels)} measures, computed on 648 empirical networks from the Index of")
    w("  Complex Networks (ICON). The encyclopedia assumes undirected, unweighted,")
    w("  loop-free graphs, so redundancy on a directed weighted transition matrix")
    w("  may differ.")
    w(f"- Off-diagonal tau: min {min(off):.2f}, median {statistics.median(off):.2f}, mean {statistics.mean(off):.2f}, max {max(off):.2f}.")
    w("- The Zoo's own code is unreleased (its Code page says so), so nothing here")
    w("  is validated against Zoo output. cograph validates each measure against")
    w("  reference packages, published worked examples, or brute-force igraph.")
    w("")
    w("## Summary")
    w("")
    w("| Group | Count |")
    w("|---|---|")
    w(f"| Zoo measures covered by cograph | {len(cov)} |")
    w(f"| Zoo measures not in cograph | {len(unc)} |")
    w(f"| ... of which rank-identical to a cograph measure (tau >= 0.99) | {len(identical)} |")
    w(f"| ... near-duplicate (0.90 <= tau < 0.99) | {len(near)} |")
    w(f"| ... some new signal (0.70 <= tau < 0.90) | {len(some)} |")
    w(f"| ... real new information (tau < 0.70) | {len(novel)} |")
    w(f"| cograph measures with no Zoo counterpart | {len(COGRAPH_ONLY)} |")
    w("")
    w("`tau` below is the **maximum** Kendall correlation between the Zoo measure and")
    w("any measure cograph already computes; `nearest` names that cograph twin (Zoo")
    w("label). A tau of 0.99 or more means the two rank nodes identically on")
    w("essentially every one of the 648 networks, so adding the measure buys nothing.")
    w("")
    w("## Covered: Zoo label to cograph measure")
    w("")
    w("| Zoo label | cograph measure |")
    w("|---|---|")
    for l in cov:
        w(f"| {l} | `{COVERED[l]}` |")
    w("")
    w("## Rank-identical to an existing measure (tau >= 0.99): not needed")
    w("")
    w("| Zoo measure | tau | nearest cograph twin |")
    w("|---|---|---|")
    for t, l, c in identical:
        w(f"| {l} | {t:.2f} | {c} (`{COVERED[c]}`) |")
    w("")
    w("## Near-duplicates (0.90 <= tau < 0.99): low value")
    w("")
    twins = {}
    for t, l, c in near:
        twins.setdefault(c, []).append(l)
    w("Grouped by the cograph twin they collapse onto. Most are re-weightings of")
    w("degree, coreness and hop distance (the gravity, k-shell-hybrid, TOPSIS and")
    w("entropy-mix families).")
    w("")
    w("| cograph twin (Zoo label) | n | Zoo measures |")
    w("|---|---|---|")
    for c, ls in sorted(twins.items(), key=lambda kv: (-len(kv[1]), kv[0])):
        w(f"| {c} (`{COVERED[c]}`) | {len(ls)} | {'; '.join(ls)} |")
    w("")
    w("## Some new signal (0.70 <= tau < 0.90)")
    w("")
    w("| Zoo measure | tau | nearest cograph twin |")
    w("|---|---|---|")
    for t, l, c in some:
        w(f"| {l} | {t:.2f} | {c} |")
    w("")
    w("## Real new information (tau < 0.70)")
    w("")
    w("| Zoo measure | tau | nearest cograph twin |")
    w("|---|---|---|")
    for t, l, c in sorted(novel, key=lambda r: (r[0], r[1])):
        w(f"| {l} | {t:.2f} | {c} |")
    w("")
    w("## Backlog")
    w("")
    w("Batch 7 (2026-09-06) shipped distance entropy, local dimension, local")
    w("information dimensionality, modularity vitality and neighborhood")
    w("connectivity; batch 8 (same day) shipped every remaining item of the")
    w("ranked list: the three Shapley games, access and hide information, rumor")
    w("centrality, community hub-bridge, entropy variation (degree and")
    w("betweenness), the s-shell index, DegreeDiscountIC, SingleDiscount and")
    w("NCVoteRank. Nothing is queued.")
    if PLANNED:
        w("")
        w("| # | Measure | Source | Note |")
        w("|---|---|---|---|")
        for k, (m, src, note) in enumerate(PLANNED, 1):
            w(f"| {k} | {m} | {src} | {note} |")
    w("")
    w("Deliberately skipped:")
    w("")
    for m, why in SKIPPED:
        w(f"- **{m}**: {why}")
    w("")
    w("## cograph measures with no Zoo counterpart")
    w("")
    w(", ".join(f"`{m}`" for m in COGRAPH_ONLY))
    w("")
    OUT.write_text("\n".join(L))
    write_lookup(labels, idx, M, cov, rows)
    # Machine-readable map for reports (tmp/zoo-coverage-matrix.Rmd reads it).
    with (HERE / "coverage_map.csv").open("w") as fh:
        fh.write("zoo_label,cograph_measure\n")
        for l in cov:
            fh.write(f'"{l}","{COVERED[l]}"\n')
    print(f"covered {len(cov)}, uncovered {len(unc)}: identical {len(identical)}, "
          f"near {len(near)}, some {len(some)}, novel {len(novel)}")


if __name__ == "__main__":
    main()
