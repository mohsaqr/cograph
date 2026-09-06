# Regenerates docs/CENTRALITY-CROSS-COVERAGE.md.
#
# How many of the Centrality Zoo's measures does each R (or Python) package
# put within reach, and what does cograph add over all of them together?
# The mapping lives in docs/zoo/package_measures.csv, curated by function
# name and bridged to Zoo labels through docs/zoo/coverage_map.csv, which
# cograph's own coverage generator writes.
#
#   Rscript docs/zoo/cross_coverage.R

suppressMessages(devtools::load_all(".", quiet = TRUE))
pm <- utils::read.csv("docs/zoo/package_measures.csv", stringsAsFactors = FALSE)
map <- utils::read.csv("docs/zoo/coverage_map.csv", stringsAsFactors = FALSE)
map$cograph <- sub(" .*$", "", map$cograph_measure)
zoo <- jsonlite::fromJSON(gzfile("docs/zoo/correlation.json.gz"))

known <- list_centralities()$measure
batch10 <- c("local_efficiency", "s_core", "fragmentation", "kpath", "epc")
near_file <- "docs/zoo/near_duplicates.csv"
nd <- if (file.exists(near_file)) {
  utils::read.csv(near_file, stringsAsFactors = FALSE)
} else {
  NULL
}
pm <- pm[nzchar(trimws(pm$cograph)), ]
unknown <- setdiff(setdiff(pm$cograph, "-"), known)
if (length(unknown)) {
  stop("package_measures.csv names measures cograph does not have: ",
       paste(unknown, collapse = ", "))
}
zoo_of <- function(measures) {
  unique(map$zoo_label[map$cograph %in% setdiff(measures, "-")])
}
pkgs <- sort(unique(pm$package))
per <- data.frame(
  package = pkgs,
  functions = vapply(pkgs, function(p) sum(pm$package == p), integer(1)),
  zoo_measures = vapply(pkgs, function(p) length(zoo_of(pm$cograph[pm$package == p])),
                        integer(1)),
  stringsAsFactors = FALSE
)
per <- per[order(-per$zoo_measures), ]
cog <- map$zoo_label
others <- zoo_of(pm$cograph)
only_cograph <- setdiff(cog, others)

# Zoo labels cograph does not implement, binned by their nearest implemented
# measure. Same rule as docs/zoo/zoo_coverage.py, recomputed here so the two
# documents cannot drift apart silently.
zoo_mat <- zoo$matrix
dimnames(zoo_mat) <- list(zoo$labels, zoo$labels)
uncovered <- setdiff(zoo$labels, cog)
nearest_tau <- vapply(uncovered, function(l) max(zoo_mat[l, cog]), numeric(1))
zoo_breaks <- c(-1, 0.70, 0.90, 0.99, 1.01)
zoo_labels <- c("tau < 0.70", "0.70 <= tau < 0.90", "0.90 <= tau < 0.99",
                "tau >= 0.99")
zoo_bins <- data.frame(
  label = rev(zoo_labels),
  n = rev(as.integer(table(cut(nearest_tau, zoo_breaks, labels = zoo_labels)))),
  stringsAsFactors = FALSE
)
stopifnot("Zoo bins must account for every label" =
            sum(zoo_bins$n) + length(cog) == length(zoo$labels))

# Each counted function of the other packages: computed by centrality(), by
# another cograph verb, or not at all.
at_other_level <- grepl("cograph has [a-z_]+\\(\\)", pm$note)
pkg_state <- ifelse(pm$cograph != "-", "in_centrality",
                    ifelse(at_other_level, "elsewhere", "not_covered"))
pkg_tab <- as.data.frame.matrix(table(pm$package, pkg_state))
for (col in c("in_centrality", "elsewhere", "not_covered")) {
  if (is.null(pkg_tab[[col]])) pkg_tab[[col]] <- 0L
}
pkg_tab$package <- rownames(pkg_tab)
pkg_tab$total <- pkg_tab$in_centrality + pkg_tab$elsewhere +
  pkg_tab$not_covered
pkg_tab <- pkg_tab[order(-pkg_tab$total), ]

# Each cograph measure by its single closest partner.
own_bins <- NULL
if (!is.null(nd)) {
  pool_all <- sort(unique(c(nd$measure_1, nd$measure_2)))
  closest <- vapply(pool_all, function(m) {
    max(abs(nd$tau[nd$measure_1 == m | nd$measure_2 == m]))
  }, numeric(1))
  own_labels <- c("tau < 0.70", "0.70 <= tau < 0.90", "0.90 <= tau < 0.95",
                  "0.95 <= tau < 0.99", "tau >= 0.99")
  own_bins <- data.frame(
    label = rev(own_labels),
    n = rev(as.integer(table(cut(closest, c(-1, 0.70, 0.90, 0.95, 0.99, 1.01),
                                 labels = own_labels)))),
    stringsAsFactors = FALSE
  )
}

L <- character(0)
w <- function(...) L <<- c(L, paste0(...))
w("# Cross-package coverage of the Centrality Zoo")
w("")
w("Coverage of the ", length(zoo$labels), " node measures catalogued in the")
w("[Centrality Zoo](https://centralityzoo.github.io/) (Shvydun 2025) by")
w("cograph and by nine other centrality packages. Counting is by measure,")
w("not by function: two functions for the same measure count once, and a")
w("function whose measure the Zoo does not catalogue is not counted.")
w("")
w("| Package | Centrality functions counted | Zoo measures reachable |")
w("|---|---|---|")
w("| **cograph** | ", length(known), " | **", length(cog), "** |")
for (i in seq_len(nrow(per))) {
  w("| ", per$package[i], " | ", per$functions[i], " | ", per$zoo_measures[i], " |")
}
w("")
w("The nine other packages together reach ", length(others), " distinct Zoo")
w("measures. cograph reaches ", length(cog), "; ", length(only_cograph),
  " of those are in no other package.")
w("")
w("## Counts")
w("")
w("### The Zoo's 349 node measures")
w("")
w("`tau` is the largest average Kendall rank correlation between the Zoo")
w("measure and any measure cograph implements, read from the Zoo's own")
w("matrix over its 648 networks.")
w("")
w("| Status | Count |")
w("|---|---|")
w("| Implemented in cograph | ", length(cog), " |")
for (i in seq_len(nrow(zoo_bins))) {
  w("| Not implemented, nearest ", zoo_bins$label[i], " | ",
    zoo_bins$n[i], " |")
}
w("| **Total** | **", length(zoo$labels), "** |")
w("")
w("### The ", nrow(pm), " functions counted in the other packages")
w("")
w("| Package | In `centrality()` | Elsewhere in cograph | Not in cograph |",
  " Total |")
w("|---|---|---|---|---|")
for (i in seq_len(nrow(pkg_tab))) {
  w("| ", pkg_tab$package[i], " | ", pkg_tab$in_centrality[i], " | ",
    pkg_tab$elsewhere[i], " | ", pkg_tab$not_covered[i], " | ",
    pkg_tab$total[i], " |")
}
w("| **Total** | **", sum(pkg_tab$in_centrality), "** | **",
  sum(pkg_tab$elsewhere), "** | **", sum(pkg_tab$not_covered), "** | **",
  sum(pkg_tab$total), "** |")
w("")
w("\"Elsewhere in cograph\" means the measure exists as a separate verb")
w("because it is not node-level: a set, a pair or the graph as a whole.")
if (!is.null(nd)) {
  w("")
  w("### cograph's ", length(known), " measures, by nearest other measure")
  w("")
  w("Average Kendall tau between every pair of cograph measures on 19 real")
  w("undirected networks (`docs/zoo/near_duplicates.R`). Each measure is")
  w("placed by its single closest partner.")
  w("")
  w("| Nearest partner at | Count |")
  w("|---|---|")
  for (i in seq_len(nrow(own_bins))) {
    w("| ", own_bins$label[i], " | ", own_bins$n[i], " |")
  }
  w("| Not in the matrix (directed-only, `NA` here) | ",
    length(known) - length(unique(c(nd$measure_1, nd$measure_2))), " |")
  w("| **Total** | **", length(known), "** |")
}
w("")
w("## Measures in cograph and in no other package")
w("")
w("| Zoo measure | cograph measure |")
w("|---|---|")
for (z in sort(only_cograph)) {
  w("| ", z, " | `", sub(" .*$", "", map$cograph_measure[map$zoo_label == z]), "` |")
}
w("")
w("## Measures in another package and not in `centrality()`")
w("")
w("Some of these cograph provides as a separate verb rather than a column")
w("of `centrality()`, because the measure is not node-level.")
w("")
gaps <- pm[pm$cograph == "-", ]
w("| Package | Function | Note |")
w("|---|---|---|")
for (i in seq_len(nrow(gaps))) {
  w("| ", gaps$package[i], " | `", gaps$fun[i], "` | ", gaps$note[i], " |")
}
w("")
w("## Measures added to `centrality()` from other packages")
w("")
w("Five node measures available in other packages and not in")
w("`centrality()`, each implemented from its source paper and verified")
w("against the package that provides it")
w("(`local_testing_and_equivalence/batch10/run_equivalence.R`).")
w("")
w("| Package | Function | cograph measure | Checked against |")
w("|---|---|---|---|")
w("| brainGraph | `efficiency(type = \"local\")` | `local_efficiency` | ",
  "brainGraph and networkx, 25 random graphs |")
w("| brainGraph | `s_core` | `s_core` | ",
  "`igraph::coreness()` unweighted, brute force weighted |")
w("| keyplayer | `fragment` | `fragmentation` | ",
  "`keyplayer::fragment()` on 25 random graphs |")
w("| sna | `kpath.census` | `kpath` | ",
  "`sna::kpath.census()`, k = 2 and 3, directed and undirected |")
w("| centiserve, CINNA | `epc` | `epc` | ",
  "exact bond-percolation mean, and `centiserve::epc()` up to its run count |")
w("")
w("Two of the five differ from a reference implementation, and both help")
w("pages state the difference. `s_core` reports the strength threshold of")
w("Eidsaa & Almaas; `brainGraph::s_core()` reports the peeling round.")
w("`local_efficiency` measures the neighbour distances inside the induced")
w("subgraph; `igraph::local_efficiency()` measures them through the rest of")
w("the network.")
w("")
w("## Measures cograph provides under another name")
w("")
w("Three functions in other packages compute a measure `centrality()`")
w("provides under a different name. Each equivalence is verified by a test")
w("in `tests/testthat/test-centrality-batch10.R`.")
w("")
w("| Package | Function | cograph measure | Relationship |")
w("|---|---|---|---|")
w("| centiserve | `closeness.latora` | `harmonic` | identical |")
w("| centiserve | `communibet` | `communicability_betweenness` | ",
  "identical (both are Estrada, Higham & Hatano 2009 normalised) |")
w("| brainGraph | `efficiency(type = \"nodal\")` | `harmonic` | ",
  "identical after dividing by *n* - 1 |")
w("")
w("## Rank correlation between cograph measures")
w("")
if (!is.null(nd)) {
  partner_of <- function(m) {
    rows <- nd[nd$measure_1 == m | nd$measure_2 == m, ]
    rows$other <- ifelse(rows$measure_1 == m, rows$measure_2, rows$measure_1)
    rows <- rows[!rows$other %in% batch10, ]
    rows <- rows[order(-abs(rows$tau)), ]
    rows[1, c("other", "tau", "networks")]
  }
  w("Kendall tau between every pair of measures, computed on ",
    max(nd$networks), " real")
  w("undirected networks and averaged over them (`docs/zoo/near_duplicates.R`).")
  w("The first table gives, for each of the five measures added from other")
  w("packages, the closest measure already in `centrality()`.")
  w("")
  w("| Added measure | Closest measure already present | tau |")
  w("|---|---|---|")
  for (m in batch10) {
    if (!any(nd$measure_1 == m | nd$measure_2 == m)) next
    best <- partner_of(m)
    w("| `", m, "` | `", best$other, "` | ", sprintf("%.2f", best$tau), " |")
  }
  w("")
  w("")
  w("These networks are unweighted. With unit weights `s_core` equals the")
  w("k-core number by construction, as does `weighted_kshell`, so the 1.00")
  w("against `coreness` is a property of the definition. Both differ from")
  w("`coreness` only on weighted input.")
  w("")
  strong <- nd[abs(nd$tau) >= 0.99, ]
  shown <- utils::head(strong, 40)
  w("Across the whole catalogue ", nrow(strong), " pairs of measures reach ",
    "|tau| >= 0.99 on")
  w("these networks. The ", nrow(shown), " closest:")
  w("")
  w("| Measure | Measure | tau |")
  w("|---|---|---|")
  for (i in seq_len(nrow(shown))) {
    w("| `", shown$measure_1[i], "` | `", shown$measure_2[i], "` | ",
      sprintf("%.3f", shown$tau[i]), " |")
  }
  if (nrow(strong) > nrow(shown)) {
    w("")
    w("The remaining ", nrow(strong) - nrow(shown), " are in ",
      "`docs/zoo/near_duplicates.csv`, which also carries every weaker pair.")
  }

  # --- how many distinct measures is that, once twins are collapsed? --------
  # Single-linkage components of the graph whose edges are the pairs above a
  # threshold: two measures land in the same group when a chain of near-twins
  # connects them. Label propagation to a fixed point, no recursion.
  groups_at <- function(th) {
    e <- nd[abs(nd$tau) >= th, ]
    lab <- stats::setNames(seq_along(pool), pool)
    if (nrow(e) == 0L) return(lab)
    repeat {
      low <- pmin(lab[e$measure_1], lab[e$measure_2])
      upd <- tapply(c(low, low), c(e$measure_1, e$measure_2), min)
      nxt <- lab
      nxt[names(upd)] <- pmin(lab[names(upd)], upd)
      if (identical(nxt, lab)) break
      lab <- nxt
    }
    lab
  }
  pool <- sort(unique(c(nd$measure_1, nd$measure_2)))
  thresholds <- c(0.99, 0.95, 0.90, 0.80)
  tab <- lapply(thresholds, function(th) {
    lab <- groups_at(th)
    sizes <- table(lab)
    data.frame(tau = th, groups = length(sizes),
               singletons = sum(sizes == 1L), largest = max(sizes))
  })
  tab <- do.call(rbind, tab)
  w("")
  w("### Distinct measures after collapsing near-duplicates")
  w("")
  w("Chains of near-duplicates collapsed into single-linkage groups, and")
  w("the groups counted. Of the ", length(known), " measures, ",
    length(pool), " are in this matrix; the")
  w("other ", length(known) - length(pool), " are directed-only and return ",
    "`NA` on these undirected networks.")
  w("")
  w("| Twins at | Distinct groups | Measures with no twin | Largest group |")
  w("|---|---|---|---|")
  for (i in seq_len(nrow(tab))) {
    w("| tau >= ", sprintf("%.2f", tab$tau[i]), " | **", tab$groups[i],
      "** | ", tab$singletons[i], " | ", tab$largest[i], " |")
  }
  w("")
  lab <- groups_at(0.95)
  fams <- split(names(lab), lab)
  fams <- fams[order(-lengths(fams))]
  groups <- do.call(rbind, lapply(seq_along(fams), function(i) {
    data.frame(group = i, size = length(fams[[i]]), measure = fams[[i]],
               stringsAsFactors = FALSE)
  }))
  utils::write.csv(groups, "docs/zoo/measure_groups.csv", row.names = FALSE)
  w("At the 0.95 cut the ", length(pool), " measures fall into ",
    tab$groups[2], " groups: ", tab$singletons[2], " of one member and ",
    sum(lengths(fams) > 1L), " of more")
  w("than one. The membership is listed below and in ",
    "`docs/zoo/measure_groups.csv`.")

  # What the members of a group share, keyed by the group's first member in
  # alphabetical order. Read off each measure's own definition; see the
  # caveats.
  basis <- c(
    average_distance = "sums of shortest-path distances, or their reciprocals",
    authority = "the leading eigenvector of the adjacency matrix, or a function of its spectrum",
    current_flow_closeness = "random walks, effective resistance and the Laplacian spectrum",
    coreness = "peeling by degree or strength",
    diffusion = "quantities built from the two- or three-step neighbourhood",
    betweenness = "shortest paths passing through the node",
    degree = "the count or sum of adjacent edges",
    katz = "counts of short walks or paths, and so of degree and neighbour degree",
    alpha = "solutions of (I - alpha A) x = b",
    flow_coefficient = "linkage among the node's neighbours; on an undirected graph one is 1 minus the other, hence the negative tau",
    hindex_strength = "an h-index over neighbour degrees",
    lac = "mean degree inside the ego network",
    local_bridging = "no shared construction; the agreement is empirical"
  )
  multi <- fams[lengths(fams) > 1L]
  w("")
  w("### Groups of two or more at |tau| >= 0.95")
  w("")
  w("| Size | Measures | What the members share |")
  w("|---|---|---|")
  for (g in multi) {
    key <- sort(g)[1]
    w("| ", length(g), " | ", paste0("`", sort(g), "`", collapse = ", "),
      " | ", basis[[key]] %||% "", " |")
  }
  w("")
  w("### Measures with no partner at |tau| >= 0.95")
  w("")
  w("Each measure with its single closest partner and that tau. A negative")
  w("tau is the same ranking reversed, so the grouping uses |tau|:")
  w("`average_distance` and `closeness` sit at -1.00 and form one group.")
  w("")
  alone <- sort(unlist(fams[lengths(fams) == 1L], use.names = FALSE))
  partner <- function(m) {
    rows <- nd[nd$measure_1 == m | nd$measure_2 == m, ]
    rows$other <- ifelse(rows$measure_1 == m, rows$measure_2, rows$measure_1)
    rows[which.max(abs(rows$tau)), c("other", "tau")]
  }
  w("| Measure | Closest other measure | tau |")
  w("|---|---|---|")
  for (m in alone) {
    best <- partner(m)
    w("| `", m, "` | `", best$other, "` | ", sprintf("%.2f", best$tau), " |")
  }
  w("")
  w("### Measures absent from the matrix")
  w("")
  w("Directed-only measures, which return `NA` on the undirected networks")
  w("the tau run uses, so they have no measured partner:")
  w("")
  w(paste0("`", sort(setdiff(known, pool)), "`", collapse = ", "), ".")
} else {
  w("Not computed: run `Rscript docs/zoo/near_duplicates.R` first.")
}
w("")
w("## Method")
w("")
w("- Package function to cograph measure is mapped by name in")
w("  `docs/zoo/package_measures.csv`. The mapping is checked at build time:")
w("  every cograph name in it must be one `list_centralities()` reports.")
w("- Reachability is not equivalence: two packages can implement the same")
w("  measure and return different values. Documented cases include")
w("  `?centrality_dmnc` against centiserve and the NetworkX divergence in")
w("  `?group_centrality`.")
w("- Some packages reach a measure through a wrapper rather than their own")
w("  code. CINNA dispatches to igraph and centiserve, so its count overlaps")
w("  theirs almost entirely.")
w("- The Zoo lists a few measures twice under different names, effective")
w("  size among them, so a package can reach more Zoo labels than it has")
w("  functions. The convention applies to every row, cograph's included.")
w("- The \"what the members share\" column is read off the measures' own")
w("  definitions. It is the only column in this document that is not a")
w("  computed value.")
w("- The grouping is single-linkage: a group is a chain of near-duplicates,")
w("  not a set in which every pair is close. Two measures at opposite ends")
w("  of the 16-member distance group can correlate well below 0.95 with")
w("  each other.")
w("- Every measure is evaluated at its default arguments, which matters")
w("  for the tunable ones. `delta_closeness` at its default exponent of 1")
w("  is harmonic centrality over n - 1, so it sits at tau 1.00 with the")
w("  distance group; it separates from that group only at other")
w("  exponents. The same holds for `delta_betweenness` at delta = 0 and")
w("  for `gravity` under a different mass or radius.")
w("- Two tau sources appear in this document. The Zoo bands come from the")
w("  Zoo's matrix over its 648 networks and its implementations; the")
w("  cograph-to-cograph tau comes from `docs/zoo/near_duplicates.R`, over")
w("  19 networks and cograph's implementations.")
w("- Measures a package implements that the Zoo does not catalogue are not")
w("  counted. This is a count of Zoo coverage, not of package size.")
w("- Generated by `docs/zoo/cross_coverage.R`.")
writeLines(L, "docs/CENTRALITY-CROSS-COVERAGE.md")
cat(sprintf("cograph %d | union of others %d | unique to cograph %d\n",
            length(cog), length(others), length(only_cograph)))
print(per, row.names = FALSE)
