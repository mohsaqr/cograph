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
    # One implementation covers both Zoo labels: 2.279 neighbor distance
    # centrality is 2.280 neighborhood centrality at theta = degree, n = 2,
    # a = 0.2, which are the cograph defaults.
    "Neighbor distance centrality": "neighbor_distance",
    "Neighborhood centrality": "neighbor_distance",
    # Zoo 2.324 transcribes Kivimaki et al. (2016) faithfully; the sibling
    # label 2.325 "Randomized shortest paths (RSP) net betweenness" is
    # equation (16) of the same paper and is NOT covered by this entry.
    "Randomized shortest paths (RSP) betweenness": "rsp_betweenness",
    # The comparison matrix labels it "Lhc method"; the encyclopedia section
    # 2.221 heads the same measure "Lhc index". Implemented from Wang et al.
    # (2021) rather than as the Zoo prints it: the Zoo normalises the
    # triangle share by "Delta, the total number of triangular structures",
    # where the paper divides by TNTS = sum_u NTS(u) = 3 Delta and says so
    # explicitly ("the total number of triangle structure exists in the
    # network are 1/3 * TNTS").
    "Lhc method": "lhc",
    # Zoo 2.180 transcribes Friedkin (1991) faithfully up to renaming the
    # left eigenvector c to v. It is NOT covered by the older `markov`
    # measure, which the Zoo's nearest-baseline column suggested: `markov`
    # omits the a_ii = 1 self-loop the source mandates AND divides by n
    # rather than n - 1, so the two differ in rank order, not only in
    # scale. The sibling labels 2.179 "Total effects centrality (TEC)" and
    # "Mediative Effects Centrality (MEC)" are equations (18) and (21)-(23)
    # of the same paper and are NOT covered by this entry.
    "Immediate Effects Centrality (IEC)": "iec",
    # Zoo 2.62 transcribes Liu et al. (2016) equations (1)-(3) correctly,
    # including lambda = p/2 + 1; no divergence was found between the Zoo,
    # the original and the Almasi & Hu (2019) reproduction. The trap is not
    # the Zoo but the PDF text layer, which renders the stacked fraction as
    # "lambda = 2p + 1"; the original was read from page images.
    "Degree and Importance of Lines (DIL)": "dil",
    # THE ZOO MIS-CITES THIS ONE. Zoo 2.381 gives reference [350], Sheng et
    # al. (2020) Physica A 541:123262, which is the global-and-local-
    # structure measure of Zoo 2.149 and a different object. The formulas
    # printed at 2.381 are equations (2), (5), (6) and (7) of a DIFFERENT,
    # fully open-access Sheng et al. paper, Algorithms 13(11):280,
    # doi:10.3390/a13110280, which is what cograph implements. The Zoo also
    # omits the s(a, a) = 1 base case of equation (4) and says nothing about
    # the recursion's domain; both are load-bearing.
    "Trust-PageRank": "trust_pagerank",
    "Local neighbor contribution (LNC)": "lnc",
    # Implemented from Chen et al. (2014), not as Zoo 2.215 prints it: the
    # Zoo drops the "1 +" from E_i and divides K_i by max_l K_l instead of
    # by N, reproducing neither printed value of the paper's figure 1.
    "KED": "ked",
    # One source, two labels: HCC is equations (3)-(4) of Liu and Zheng
    # (2023) and EHCC its equation (5), the closed-neighbourhood sum.
    "Hybrid Characteristic Centrality (HCC)": "hcc",
    "Extended hybrid characteristic centrality (EHCC)": "ehcc",
    "Iterative resource allocation (IRA)": "ira",
    "Improved iterative resource allocation (IIRA)": "iira",
    "Relative entropy": "relative_entropy",
    "DK-based gravity model (DKGM)": "dkgm",
    "Mixed gravitational centrality": "mixed_gravity",
    "Extended mixed gravitational centrality": "extended_mixed_gravity",
    "Multi-characteristics gravity model (MCGM)": "mcgm",
    "SpectralRank": "spectralrank",
    "ControlRank": "controlrank",
    "Map Equation Centrality (MEC)": "map_equation",
    "Node and Neighbor Layer Information (NINL) centrality": "ninl",
    "Extended LBC": "extended_local_bridging",
    "BG-index": "beta_measure",
    "beta-measure": "beta_measure",
    "Expected force (ExFm)": "modified_expected_force",
    "Proximal betwenness": "proximal_betweenness",
    "Bridging capital": "bridging_capital",
    "Coleman-Theil disorder index": "coleman_theil",
    "X-degree centrality": "x_degree",
    "LineRank": "linerank",
    "Random walk decay": "random_walk_decay",
    "Graph regularization centrality (GRC)": "graph_regularization",
    "Degree": "degree", "Closeness": "closeness", "Eccentricity": "eccentricity",
    "k-shell": "coreness", "Harmonic": "harmonic", "Diffusion Degree": "diffusion",
    "Leverage": "leverage", "m-reach": "kreach", "Katz": "katz (also alpha)",
    "Lin's index": "lin", "Decay": "decay", "Residual closeness": "residual_closeness",
    "Lobby index": "lobby", "Entropy": "entropy",
    "Semi-local ranking (SLC)": "semilocal", "ClusterRank": "clusterrank",
    "BottleNeck": "bottleneck", "Centroid": "centroid", "MNC": "mnc", "DMNC": "dmnc",
    "Closeness vitality": "closeness_vitality", "Integration": "integration",
    "Expected force (ExF)": "expected_force", "Gil-Schmidt Power Index": "gilschmidt",
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
    "Localized bridging centrality": "localized_bridging",
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
    # Batch 9 (2026-09-06)
    "Community-based centrality (CbC)": "community_based [batch 9]",
    "Comm Centrality": "comm_centrality [batch 9]",
    "Community-based mediator (CbM)": "community_mediator [batch 9]",
    "Local dimension (LD)": "local_dimension_fixed [batch 9]",
    "Fuzzy local dimension (FLD)": "fuzzy_local_dimension [batch 9]",
    "Local volume dimension (LVD)": "local_volume_dimension [batch 9]",
    "WVoteRank": "wvoterank [batch 9]",
    "EnRenew": "enrenew [batch 9]",
    "VoteRank++": "voterank_plus [batch 9]",
    "Node contraction (IMC)": "node_contraction [batch 9]",
    "Improved IMC": "node_contraction_improved [batch 9]",
    "Two-way random walk betweenness (2RW)": "two_way_rw [batch 9]",
    "Heatmap centrality": "heatmap [batch 9]",
    "Flow coefficient": "flow_coefficient [batch 9]",
    "Local entropy (LE)": "local_entropy [batch 9]",
    "Weighted h-index": "weighted_h_index [batch 9]",
    "Redundancy": "redundancy [batch 9]",
    "Weighted k-shell decomposition (Wks)": "weighted_kshell [batch 9]",
    "Renewed coreness": "renewed_coreness [batch 9]",
    "Geodesic k-path": "geodesic_kpath [batch 9]",
    "Distance-weighted fragmentation": "fragmentation [batch 10]",
    "Length-scaled betweenness": "length_scaled_betweenness [batch 11]",
    "delta-betweenness": "delta_betweenness [batch 11]",
    "delta-closeness": "delta_closeness [batch 11]",
    "Egocentric betweenness": "ego_betweenness [batch 11]",
    "k-betweenness": "betweenness (with cutoff)",
    "Gravity model": "gravity (gravity_mass = degree, gravity_radius = NULL)",
    "Local gravity model": "gravity (gravity_mass = degree, gravity_radius = auto)",
    # Batch 12: formula and independent numerical checks, not tau aliases.
    "LocalRank": "semilocal",
    "k-truss number": "truss [batch 12]",
    "Mixed Degree Decomposition (MDD)": "mdd [batch 12]",
    "Bridging coefficient": "bridging_coefficient [batch 12]",
    "Godfather index": "godfather [batch 12]",
    "Support": "support [batch 12]",
    "Volume centrality": "volume [batch 13]",
    "MCC": "mcc [batch 13]",
    "Diffusion centrality": "diffusion_centrality [batch 14]",
    "Dynamical importance": "dynamical_importance [batch 14]",
    "Dynamics-sensitive (DS) centrality": "dynamics_sensitive [batch 15]",
    "Malatya centrality": "malatya [batch 15]",
    "Resistance curvature": "resistance_curvature [batch 16]",
    "Extended neighborhood coreness": "extended_coreness [batch 17]",
    "Extended gravity centrality": "extended_gravity [batch 17]",
    "Clustering degree algorithm (CDA)": "cda [batch 18]",
    "Improved closeness centrality (ICC)": "improved_closeness [batch 19]",
    "Exogenous centrality": "exogenous [batch 20]",
    "Global Structure Model (GSM)": "global_structure [batch 21]",
    "Hybrid Global Structure Model (H-GSM)": "hybrid_global_structure [batch 21]",
    "Improved global structure model (IGSM)": "improved_global_structure [batch 21]",
    "Weighted LeaderRank": "weighted_leaderrank [batch 22]",
    "Adaptive LeaderRank": "adaptive_leaderrank [batch 23]",
}

# cograph measures with no Zoo counterpart (for completeness).
COGRAPH_ONLY = [
    "local_bridging",  # Legacy inverse-degree product, not Nanda-Kotz LBC.
    "expected",  # Neighbor-degree sum, not Lawyer Expected Force.
    "strength", "authority", "hub", "power", "radiality", "dangalchev",
    "generalized_closeness", "harary", "average_distance", "barycenter",
    "wiener", "lac", "onion", "reaching_local", "trophic_level",
    "prestige_domain", "prestige_domain_proximity",
    "brokerage_coordinator", "brokerage_itinerant", "brokerage_representative",
    "brokerage_gatekeeper", "brokerage_liaison",
    "expected_influence_1", "expected_influence_2",
    # Batch 10. The Zoo lists "k-path" and "Efficiency centrality (EffC)",
    # but neither definition could be pinned to the source, so `kpath` and
    # `local_efficiency` are counted here rather than as Zoo coverage.
    "local_efficiency", "s_core", "kpath", "epc",
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
    ("DegreePunishment", "source article (Wang, Su, Zhao & Yi 2016) unobtainable; Zoo entry alone leaves beta_c and the seed rule open"),
    ("Improved WVoteRank", "source article (Kumar & Panda 2022) unobtainable; Zoo formula has three unresolved switches"),
    ("Local degree dimension (LDD)", "source article (Zhong, Zhang & Deng 2022) unobtainable; the Zoo formula's bracketing is ambiguous"),
    ("Multi-local dimension (MLD)", "for every q outside (0, 1) it is q/(q-1) times local_dimension, reversed inside (0, 1), and q = 1 is local_information_dimension over the full range; nothing new to rank"),
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


def call_for(measure, zoo_label=None):
    overrides = {
        "Gravity model": 'centrality(x, measures = "gravity", gravity_mass = "degree", gravity_radius = NULL)',
        "Local gravity model": 'centrality(x, measures = "gravity", gravity_mass = "degree", gravity_radius = "auto")',
        "k-betweenness": 'centrality(x, measures = "betweenness", cutoff = k)',
        "Diffusion centrality": 'centrality(x, measures = "diffusion_centrality", diffusion_q = 1, diffusion_steps = 3)',
        "Dynamics-sensitive (DS) centrality": 'centrality(x, measures = "dynamics_sensitive", ds_beta = 0.1, ds_mu = 1, ds_steps = 5)',
    }
    if zoo_label in overrides:
        return f'`{overrides[zoo_label]}`'
    return f'`centrality(x, measures = "{clean_name(measure)}")`'


def write_lookup(labels, idx, M, cov, rows):
    """Reader-facing lookup: what cograph has for each Zoo measure."""
    nearest = {l: (t, c) for t, l, c in rows}
    by_name = lambda l: l.lower()
    substitute = sorted((l for l in nearest if nearest[l][0] >= 0.90), key=by_name)
    absent = sorted((l for l in nearest if nearest[l][0] < 0.90), key=by_name)
    identical = sum(1 for l in substitute if nearest[l][0] >= 0.99)

    L = []
    w = L.append
    w("Every measure in the [Centrality Zoo](https://centralityzoo.github.io/comparison/)")
    w(f"({len(labels)} measures, Shvydun 2025) appears exactly once below. `tau` is the")
    w("average Kendall rank correlation between Zoo implementations over 648")
    w("empirical networks. A mapped cograph name identifies a corresponding")
    w("definition; these are not comparisons against cograph outputs. Values are")
    w("rounded: displayed 1.00 does not prove exact rank or numerical equivalence.")
    w("")
    w("| Section | Meaning | Count |")
    w("|---|---|---|")
    w(f"| Available | cograph implements the measure itself | {len(cov)} |")
    w(f"| Correlated candidate (tau >= 0.90) | not implemented; a mapped Zoo measure has high average rank correlation | {len(substitute)} |")
    w(f"| Not available (tau < 0.90) | not implemented; nearest mapped Zoo measure has lower average correlation | {len(absent)} |")
    w("")
    w(f"Only {len(cov)} of the {len(labels)} Zoo labels are mapped as implemented.")
    w(f"Another {len(substitute)} have a correlated candidate, including {identical}")
    w("at tau >= 0.99. Correlation does not establish interchangeable measures.")
    w("")
    w("Caveat: the Zoo computed tau on undirected, unweighted networks, so")
    w("agreement on a directed or weighted network may be lower. Rank agreement")
    w("does not establish numerical equivalence or accuracy. No fixed conversion")
    w("between Kendall and Spearman correlation is assumed.")
    w("")
    w("## Available in cograph")
    w("")
    w("| Zoo measure | cograph measure | Call |")
    w("|---|---|---|")
    for l in sorted(cov, key=by_name):
        m = clean_name(COVERED[l])
        w(f"| {l} | `{m}` | {call_for(m, l)} |")
    w("")
    w("## Correlated candidates (tau >= 0.90)")
    w("")
    w("These measures are not implemented. The nearest mapped Zoo measure is")
    w("a research lead, not a verified substitute. Average correlation alone")
    w("cannot establish equivalence on an individual network.")
    w("")
    w("| Zoo measure | Correlated cograph candidate | Call | tau | Evidence |")
    w("|---|---|---|---|---|")
    for l in substitute:
        t, c = nearest[l]; m = clean_name(COVERED[c])
        how = "high average rank correlation; not equivalence"
        w(f"| {l} | `{m}` | {call_for(m, c)} | {t:.2f} | {how} |")
    w("")
    w("## Not available (tau < 0.90)")
    w("")
    w("These measures are not implemented. The nearest mapped Zoo measure and")
    w("its average tau provide comparison context, without proving equivalence.")
    w("")
    w("| Zoo measure | Nearest cograph measure | tau |")
    w("|---|---|---|")
    for l in absent:
        t, c = nearest[l]; m = clean_name(COVERED[c])
        w(f"| {l} | `{m}` | {t:.2f} |")
    w("")
    body = "\n".join(L)
    LOOKUP.write_text(
        "# Looking for a centrality from the Zoo? Here is what cograph has\n\n"
        + body
        + "\nGenerated by `docs/zoo/zoo_coverage.py`; see `CENTRALITY-ZOO-COVERAGE.md`\n"
        + "for the same data with nearest-measure detail and the skipped list.\n")
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
    w(f"| ... very high average rank correlation (tau >= 0.99) | {len(identical)} |")
    w(f"| ... high average rank correlation (0.90 <= tau < 0.99) | {len(near)} |")
    w(f"| ... moderate average rank correlation (0.70 <= tau < 0.90) | {len(some)} |")
    w(f"| ... lower average rank correlation (tau < 0.70) | {len(novel)} |")
    w(f"| cograph measures with no Zoo counterpart | {len(COGRAPH_ONLY)} |")
    w("")
    w("`tau` below is the **maximum** Kendall correlation between the Zoo measure and")
    w("any mapped Zoo measure; `nearest` names that Zoo label. These are comparisons")
    w("between Zoo implementations, not cograph outputs. Even tau >= 0.99 is only")
    w("a research lead: it does not establish mathematical or numerical equivalence.")
    w("")
    w("## Covered: Zoo label to cograph measure")
    w("")
    w("| Zoo label | cograph measure |")
    w("|---|---|")
    for l in cov:
        w(f"| {l} | `{COVERED[l]}` |")
    w("")
    w("## Very high average rank correlation (tau >= 0.99): equivalence unverified")
    w("")
    w("| Zoo measure | tau | nearest cograph twin |")
    w("|---|---|---|")
    for t, l, c in identical:
        w(f"| {l} | {t:.2f} | {c} (`{COVERED[c]}`) |")
    w("")
    w("## High average rank correlation (0.90 <= tau < 0.99)")
    w("")
    twins = {}
    for t, l, c in near:
        twins.setdefault(c, []).append(l)
    w("Grouped by the nearest mapped Zoo measure. Each candidate still requires")
    w("a definition review and independent verification before implementation.")
    w("")
    w("| cograph twin (Zoo label) | n | Zoo measures |")
    w("|---|---|---|")
    for c, ls in sorted(twins.items(), key=lambda kv: (-len(kv[1]), kv[0])):
        w(f"| {c} (`{COVERED[c]}`) | {len(ls)} | {'; '.join(ls)} |")
    w("")
    w("## Moderate average rank correlation (0.70 <= tau < 0.90)")
    w("")
    w("| Zoo measure | tau | nearest cograph twin |")
    w("|---|---|---|")
    for t, l, c in some:
        w(f"| {l} | {t:.2f} | {c} |")
    w("")
    w("## Lower average rank correlation (tau < 0.70)")
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
    w("NCVoteRank. Batch 9 (same day) shipped twenty more: the community trio")
    w("(CbC, Comm, CbM), three dimension measures, WVoteRank, EnRenew,")
    w("VoteRank++, node contraction (plain and improved), two-way random-walk")
    w("betweenness, heatmap, flow coefficient, local entropy, weighted h-index,")
    w("redundancy, weighted k-shell, renewed coreness and geodesic k-path.")
    w("Nothing is queued.")
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
