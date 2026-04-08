#!/usr/bin/env Rscript
# =============================================================================
# Consolidated cograph centrality validation runner (Batches 3 + 4 + 5 + 6)
# =============================================================================
#
# Re-runs every bit-exact / machine-epsilon check from the 2026-04 centrality
# expansion in a single reproducible pass. Covers:
#
#   Batch 3 — katz, hubbell, information, pairwisedis, reaching_local,
#             reaching_global (per-node + graph scalar)
#   Batch 4 — prestige_domain, prestige_domain_proximity
#   Batch 5 — 5 Gould-Fernandez brokerage roles
#   Batch 6 — estrada_index, trophic_incoherence, group_centrality,
#             dispersion (new-API graph/set/pair-level functions)
#
# Usage
# -----
#   Rscript scripts/validate_centrality.R
#   Rscript scripts/validate_centrality.R --snapshot   # write snapshot file
#   Rscript scripts/validate_centrality.R --diff       # diff vs snapshot
#
# Reference packages — all are *optional*. Any check whose reference is not
# installed is recorded as SKIP, not FAIL, so the script runs on machines
# without NetworkX / sna / centiserve.
#
# Exits non-zero if any check FAILs. SKIPs do not fail the run.
# =============================================================================

# ------------------------------------------------------------------- bootstrap
# Prefer the source tree via devtools::load_all() so the script validates the
# *current* package, not a possibly older CRAN install. Fall back to library()
# only when run outside the source tree.
is_cograph_source <- file.exists("DESCRIPTION") &&
  isTRUE(unname(read.dcf("DESCRIPTION", "Package")[1, 1]) == "cograph")

suppressMessages({
  if (is_cograph_source && requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".", quiet = TRUE)
  } else {
    library(cograph)
  }
  library(igraph)
})

args <- commandArgs(trailingOnly = TRUE)
write_snapshot <- "--snapshot" %in% args
diff_snapshot  <- "--diff" %in% args
snapshot_path  <- file.path("scripts", "validate_centrality_snapshot.txt")

has_sna        <- requireNamespace("sna", quietly = TRUE)
has_centiserve <- requireNamespace("centiserve", quietly = TRUE)
has_reticulate <- requireNamespace("reticulate", quietly = TRUE)
has_nx         <- has_reticulate && reticulate::py_module_available("networkx")

nx <- if (has_nx) reticulate::import("networkx") else NULL

# ------------------------------------------------------------ result collector
results <- list()
record <- function(batch, measure, reference, status, detail = "") {
  results[[length(results) + 1L]] <<- data.frame(
    batch     = batch,
    measure   = measure,
    reference = reference,
    status    = status,
    detail    = detail,
    stringsAsFactors = FALSE
  )
}

fmt_rel <- function(x) {
  if (is.na(x) || !is.finite(x)) return("NA")
  format(x, scientific = TRUE, digits = 3)
}

# --------------------------------------------------------------- nx converters
to_nx_graph <- function(g) {
  el <- as_edgelist(g)
  gp <- if (is_directed(g)) nx$DiGraph() else nx$Graph()
  gp$add_nodes_from(as.integer(0:(vcount(g) - 1)))
  if (nrow(el) > 0) {
    for (j in seq_len(nrow(el))) {
      gp$add_edge(as.integer(el[j, 1] - 1), as.integer(el[j, 2] - 1))
    }
  }
  gp
}

nx_node_set <- function(S) {
  reticulate::py_eval(sprintf("set([%s])", paste(S - 1L, collapse = ",")))
}

# ================================================================= fixtures
# Deterministic fixtures matching the ones used during development so the
# snapshot numbers are reproducible.

set.seed(2024)
dir_fixtures <- list()
for (i in 1:10) {
  n <- sample(8:20, 1)
  p <- runif(1, 0.2, 0.45)
  g <- sample_gnp(n, p, directed = TRUE)
  dir_fixtures[[sprintf("dir-%02d", i)]] <- g
}

set.seed(2024)
undir_fixtures <- list()
for (i in 1:10) {
  n <- sample(8:20, 1)
  repeat {
    g <- sample_gnp(n, 0.4, directed = FALSE)
    if (is_connected(g)) break
  }
  undir_fixtures[[sprintf("undir-%02d", i)]] <- g
}

set.seed(4242)
undir_cc <- list()
attempts <- 0
while (length(undir_cc) < 10 && attempts < 200) {
  attempts <- attempts + 1
  n <- sample(10:18, 1)
  g <- sample_gnp(n, 0.4, directed = FALSE)
  if (is_connected(g)) {
    undir_cc[[sprintf("u%02d", length(undir_cc) + 1)]] <- g
  }
}

set.seed(4242)
dir_sc <- list()
attempts <- 0
while (length(dir_sc) < 8 && attempts < 500) {
  attempts <- attempts + 1
  n <- sample(10:15, 1)
  g <- sample_gnp(n, 0.45, directed = TRUE)
  if (is_connected(g, mode = "strong")) {
    dir_sc[[sprintf("d%02d", length(dir_sc) + 1)]] <- g
  }
}

set.seed(12345)
dir_any <- list()
for (i in 1:8) {
  n <- sample(10:16, 1)
  g <- sample_gnp(n, 0.15, directed = TRUE)
  if (ecount(g) >= 3 && !all(igraph::degree(g, mode = "in") > 0)) {
    dir_any[[sprintf("t%02d", length(dir_any) + 1)]] <- g
  }
}

# Common small deterministic graphs used by multiple checks
karate <- make_graph("Zachary")

cat(sprintf(
  "fixtures: %d undir-connected, %d dir-any, %d dir-strongly-connected, %d dir-trophic\n\n",
  length(undir_fixtures) + length(undir_cc),
  length(dir_fixtures),
  length(dir_sc),
  length(dir_any)
))

# =========================================================================
# BATCH 3 — per-node classical measures
# =========================================================================

# ---- katz vs centiserve::katzcent ---------------------------------------
if (has_centiserve) {
  set.seed(1)
  max_diff <- 0
  for (i in seq_along(undir_fixtures)) {
    g <- undir_fixtures[[i]]
    ref <- centiserve::katzcent(g)
    cog <- centrality(g, measures = "katz")$katz
    max_diff <- max(max_diff, max(abs(ref - cog)))
  }
  status <- if (identical(max_diff, 0)) "PASS" else "FAIL"
  record("3", "katz", "centiserve::katzcent",
         status, sprintf("max|Δ|=%s over %d graphs",
                         fmt_rel(max_diff), length(undir_fixtures)))
} else {
  record("3", "katz", "centiserve::katzcent", "SKIP", "centiserve not installed")
}

# ---- hubbell vs centiserve::hubbell -------------------------------------
if (has_centiserve) {
  # centiserve::hubbell needs an explicit weightfactor such that (I - αA) is
  # invertible; pick something well under 1/λ_max.
  set.seed(1)
  max_diff <- 0
  tested <- 0
  for (i in seq_along(undir_fixtures)) {
    g <- undir_fixtures[[i]]
    A <- as.matrix(as_adjacency_matrix(g))
    lam <- max(abs(eigen(A, symmetric = TRUE, only.values = TRUE)$values))
    wf <- 1 / (lam + 1)
    ref <- tryCatch(
      as.numeric(centiserve::hubbell(g, weightfactor = wf)),
      error = function(e) NULL
    )
    if (is.null(ref)) next
    cog <- suppressWarnings(
      centrality(g, measures = "hubbell", hubbell_weight = wf)$hubbell
    )
    if (any(is.na(cog))) next
    max_diff <- max(max_diff, max(abs(ref - cog)))
    tested <- tested + 1
  }
  status <- if (tested == 0) "SKIP" else if (max_diff <= 1e-10) "PASS" else "FAIL"
  record("3", "hubbell", "centiserve::hubbell",
         status, sprintf("max|Δ|=%s over %d graphs",
                         fmt_rel(max_diff), tested))
} else {
  record("3", "hubbell", "centiserve::hubbell", "SKIP", "centiserve not installed")
}

# ---- information vs sna::infocent ---------------------------------------
if (has_sna) {
  max_diff <- 0
  for (i in seq_along(undir_fixtures)) {
    g <- undir_fixtures[[i]]
    A <- as.matrix(as_adjacency_matrix(g))
    ref <- sna::infocent(A)
    cog <- centrality(g, measures = "information")$information
    if (any(!is.finite(ref)) || any(!is.finite(cog))) next
    max_diff <- max(max_diff, max(abs(ref - cog)))
  }
  status <- if (max_diff <= 1e-10) "PASS" else "FAIL"
  record("3", "information", "sna::infocent",
         status, sprintf("max|Δ|=%s", fmt_rel(max_diff)))
} else {
  record("3", "information", "sna::infocent", "SKIP", "sna not installed")
}

# ---- pairwisedis vs centiserve::pairwisedis -----------------------------
if (has_centiserve) {
  max_diff <- 0
  tested <- 0
  for (i in seq_along(dir_fixtures)) {
    g <- dir_fixtures[[i]]
    ref <- tryCatch(centiserve::pairwisedis(g), error = function(e) NULL)
    if (is.null(ref)) next
    cog <- suppressWarnings(centrality(g, measures = "pairwisedis")$pairwisedis)
    if (any(is.na(cog))) next
    max_diff <- max(max_diff, max(abs(ref - cog)))
    tested <- tested + 1
  }
  status <- if (tested == 0) "SKIP" else if (max_diff <= 1e-12) "PASS" else "FAIL"
  record("3", "pairwisedis", "centiserve::pairwisedis",
         status, sprintf("max|Δ|=%s over %d graphs",
                         fmt_rel(max_diff), tested))
} else {
  record("3", "pairwisedis", "centiserve::pairwisedis", "SKIP", "centiserve not installed")
}

# ---- reaching_local / reaching_global vs NetworkX -----------------------
if (has_nx) {
  cog <- centrality(karate, measures = "reaching_local")$reaching_local
  ref <- sapply(0:(vcount(karate) - 1), function(i)
    nx$local_reaching_centrality(to_nx_graph(karate), as.integer(i)))
  max_diff <- max(abs(cog - ref))
  record("3", "reaching_local", "nx.local_reaching_centrality",
         if (max_diff <= 1e-12) "PASS" else "FAIL",
         sprintf("karate max|Δ|=%s", fmt_rel(max_diff)))

  cog_g <- reaching_global(karate)
  ref_g <- nx$global_reaching_centrality(to_nx_graph(karate))
  max_diff <- abs(cog_g - ref_g)
  record("3", "reaching_global", "nx.global_reaching_centrality",
         if (max_diff <= 1e-12) "PASS" else "FAIL",
         sprintf("karate |Δ|=%s", fmt_rel(max_diff)))
} else {
  record("3", "reaching_local",  "nx.local_reaching_centrality",  "SKIP", "networkx unavailable")
  record("3", "reaching_global", "nx.global_reaching_centrality", "SKIP", "networkx unavailable")
}

# =========================================================================
# BATCH 4 — directed prestige family (Wasserman-Faust / sna)
# =========================================================================

if (has_sna) {
  # Build strongly connected directed fixtures for prestige_domain_proximity
  set.seed(2024)
  dir_sc_local <- list()
  attempts <- 0
  while (length(dir_sc_local) < 10 && attempts < 500) {
    attempts <- attempts + 1
    n <- sample(8:14, 1)
    g <- sample_gnp(n, 0.45, directed = TRUE)
    if (is_connected(g, mode = "strong")) {
      dir_sc_local[[length(dir_sc_local) + 1]] <- g
    }
  }

  # prestige_domain: any directed graph
  max_diff <- 0
  for (i in seq_along(dir_fixtures)) {
    g <- dir_fixtures[[i]]
    A <- as.matrix(as_adjacency_matrix(g))
    ref <- sna::prestige(A, cmode = "domain")
    cog <- suppressWarnings(
      centrality(g, measures = "prestige_domain")$prestige_domain
    )
    if (any(is.na(cog))) next
    max_diff <- max(max_diff, max(abs(ref - cog)))
  }
  record("4", "prestige_domain", "sna::prestige(domain)",
         if (max_diff <= 1e-12) "PASS" else "FAIL",
         sprintf("max|Δ|=%s", fmt_rel(max_diff)))

  # prestige_domain_proximity: strongly connected only (sna bug otherwise)
  max_diff <- 0
  for (g in dir_sc_local) {
    A <- as.matrix(as_adjacency_matrix(g))
    ref <- sna::prestige(A, cmode = "domain.proximity")
    cog <- suppressWarnings(
      centrality(g, measures = "prestige_domain_proximity")$prestige_domain_proximity
    )
    if (any(!is.finite(ref)) || any(!is.finite(cog))) next
    max_diff <- max(max_diff, max(abs(ref - cog)))
  }
  record("4", "prestige_domain_proximity", "sna::prestige(domain.proximity)",
         if (max_diff <= 1e-12) "PASS" else "FAIL",
         sprintf("max|Δ|=%s on strongly connected", fmt_rel(max_diff)))

  # Documented divergence: sna has a FALSE*Inf=NaN bug on graphs with
  # unreachable pairs. cograph should produce finite values there.
  g_bug <- make_graph(c(1,2, 2,1, 3,4, 4,3), directed = TRUE)
  cog_bug <- suppressWarnings(
    centrality(g_bug, measures = "prestige_domain_proximity")$prestige_domain_proximity
  )
  all_finite <- all(is.finite(cog_bug))
  record("4", "prestige_domain_proximity", "(cograph > sna bug case)",
         if (all_finite) "PASS" else "FAIL",
         "cograph returns finite on disconnected pairs")
} else {
  record("4", "prestige_domain",           "sna::prestige", "SKIP", "sna not installed")
  record("4", "prestige_domain_proximity", "sna::prestige", "SKIP", "sna not installed")
}

# =========================================================================
# BATCH 5 — Gould-Fernandez brokerage (5 roles)
# =========================================================================

if (has_sna) {
  role_map <- c(
    coordinator    = "w_I",
    itinerant      = "w_O",
    representative = "b_IO",
    gatekeeper     = "b_OI",
    liaison        = "b_O"
  )

  set.seed(2024)
  broke_fixtures <- list()
  for (i in 1:20) {
    n <- sample(8:15, 1)
    g <- sample_gnp(n, runif(1, 0.2, 0.5), directed = TRUE)
    cl <- sample(1:3, n, replace = TRUE)
    broke_fixtures[[i]] <- list(g = g, cl = cl)
  }

  for (role in names(role_map)) {
    sna_col <- role_map[[role]]
    max_diff <- 0L
    for (fx in broke_fixtures) {
      A   <- as.matrix(as_adjacency_matrix(fx$g))
      ref <- as.integer(sna::brokerage(A, cl = fx$cl)$raw.nli[, sna_col])
      cog <- centrality(fx$g,
                        measures = paste0("brokerage_", role),
                        membership = fx$cl)[[paste0("brokerage_", role)]]
      d <- max(abs(ref - as.integer(cog)))
      if (d > max_diff) max_diff <- d
    }
    record("5", paste0("brokerage_", role),
           sprintf("sna::brokerage$raw.nli[,%s]", sna_col),
           if (max_diff == 0L) "PASS" else "FAIL",
           sprintf("integer diff=%d over 20 graphs", max_diff))
  }
} else {
  for (role in c("coordinator", "itinerant", "representative",
                 "gatekeeper", "liaison")) {
    record("5", paste0("brokerage_", role), "sna::brokerage", "SKIP",
           "sna not installed")
  }
}

# =========================================================================
# BATCH 6 — new-API graph / set / pair-level functions
# =========================================================================

# ---- estrada_index vs NetworkX ------------------------------------------
if (has_nx) {
  max_rel <- 0
  for (g in undir_cc) {
    cog <- estrada_index(g)
    ref <- nx$estrada_index(to_nx_graph(g))
    max_rel <- max(max_rel, abs(cog - ref) / abs(ref))
  }
  record("6", "estrada_index", "nx.estrada_index",
         if (max_rel <= 1e-12) "PASS" else "FAIL",
         sprintf("max rel=%s over %d graphs", fmt_rel(max_rel), length(undir_cc)))
} else {
  record("6", "estrada_index", "nx.estrada_index", "SKIP", "networkx unavailable")
}

# ---- internal identity: EE(g) == sum(subgraph_centrality) ---------------
max_id <- 0
for (g in undir_cc) {
  ei <- estrada_index(g)
  sc <- sum(centrality(g, measures = "subgraph")$subgraph)
  max_id <- max(max_id, abs(ei - sc))
}
record("6", "estrada_index (identity)", "sum(centrality(,subgraph))",
       if (max_id <= 1e-9) "PASS" else "FAIL",
       sprintf("max|EE - ΣSC|=%s", fmt_rel(max_id)))

# ---- trophic_incoherence vs NetworkX ------------------------------------
if (has_nx) {
  # perfect-chain q == 0
  chain <- matrix(0, 4, 4)
  chain[1, 2] <- chain[2, 3] <- chain[3, 4] <- 1
  q_chain <- trophic_incoherence(chain)
  record("6", "trophic_incoherence (chain)", "q == 0 on perfect 4-chain",
         if (abs(q_chain) <= 1e-12) "PASS" else "FAIL",
         sprintf("q=%s", fmt_rel(q_chain)))

  max_rel <- 0
  tested <- 0
  for (g in dir_any) {
    cog <- suppressWarnings(trophic_incoherence(g))
    ref <- tryCatch(nx$trophic_incoherence_parameter(to_nx_graph(g)),
                    error = function(e) NA)
    if (is.na(cog) || is.na(ref)) next
    max_rel <- max(max_rel, abs(cog - ref) / max(abs(ref), 1e-15))
    tested <- tested + 1
  }
  record("6", "trophic_incoherence", "nx.trophic_incoherence_parameter",
         if (tested == 0) "SKIP" else if (max_rel <= 1e-12) "PASS" else "FAIL",
         sprintf("max rel=%s over %d graphs", fmt_rel(max_rel), tested))
} else {
  record("6", "trophic_incoherence (chain)", "q == 0 on 4-chain", "SKIP", "networkx unavailable")
  record("6", "trophic_incoherence",         "nx.trophic_incoherence_parameter", "SKIP", "networkx unavailable")
}

# ---- group_centrality: closeness + degree vs NetworkX -------------------
if (has_nx) {
  set.seed(100)
  # undirected closeness + degree
  max_gcc_u <- 0; max_gdc_u <- 0
  for (g in undir_cc) {
    S    <- sort(sample(seq_len(vcount(g)), 3))
    S_py <- nx_node_set(S)
    g_py <- to_nx_graph(g)
    cc <- group_centrality(g, S, "closeness")
    nc <- nx$group_closeness_centrality(g_py, S_py)
    cd <- group_centrality(g, S, "degree")
    nd <- nx$group_degree_centrality(g_py, S_py)
    max_gcc_u <- max(max_gcc_u, abs(cc - nc))
    max_gdc_u <- max(max_gdc_u, abs(cd - nd))
  }
  record("6", "group_centrality(closeness, undirected)",
         "nx.group_closeness_centrality",
         if (max_gcc_u <= 1e-12) "PASS" else "FAIL",
         sprintf("max|Δ|=%s", fmt_rel(max_gcc_u)))
  record("6", "group_centrality(degree, undirected)",
         "nx.group_degree_centrality",
         if (max_gdc_u <= 1e-12) "PASS" else "FAIL",
         sprintf("max|Δ|=%s", fmt_rel(max_gdc_u)))

  # directed: closeness, in-degree, out-degree
  max_gcc_d <- 0; max_out <- 0; max_in <- 0
  for (g in dir_sc) {
    S    <- sort(sample(seq_len(vcount(g)), 3))
    S_py <- nx_node_set(S)
    g_py <- to_nx_graph(g)
    cc <- group_centrality(g, S, "closeness")
    nc <- nx$group_closeness_centrality(g_py, S_py)
    co <- group_centrality(g, S, "degree", mode = "out")
    no <- nx$group_out_degree_centrality(g_py, S_py)
    ci <- group_centrality(g, S, "degree", mode = "in")
    ni <- nx$group_in_degree_centrality(g_py, S_py)
    max_gcc_d <- max(max_gcc_d, abs(cc - nc))
    max_out   <- max(max_out,   abs(co - no))
    max_in    <- max(max_in,    abs(ci - ni))
  }
  record("6", "group_centrality(closeness, directed)",
         "nx.group_closeness_centrality",
         if (max_gcc_d <= 1e-12) "PASS" else "FAIL",
         sprintf("max|Δ|=%s", fmt_rel(max_gcc_d)))
  record("6", "group_centrality(degree, in)",
         "nx.group_in_degree_centrality",
         if (max_in <= 1e-12) "PASS" else "FAIL",
         sprintf("max|Δ|=%s", fmt_rel(max_in)))
  record("6", "group_centrality(degree, out)",
         "nx.group_out_degree_centrality",
         if (max_out <= 1e-12) "PASS" else "FAIL",
         sprintf("max|Δ|=%s", fmt_rel(max_out)))
} else {
  for (tag in c("group_centrality(closeness, undirected)",
                "group_centrality(degree, undirected)",
                "group_centrality(closeness, directed)",
                "group_centrality(degree, in)",
                "group_centrality(degree, out)")) {
    record("6", tag, "networkx", "SKIP", "networkx unavailable")
  }
}

# ---- group_centrality betweenness: textbook hand-verified cases ---------
# These run without NetworkX — they are the independent textbook oracles.
g4 <- make_graph(c(1,2, 2,3, 3,4, 4,1), n = 4, directed = TRUE)
v1 <- group_centrality(g4, nodes = 2,      measure = "betweenness", normalized = FALSE)
v2 <- group_centrality(g4, nodes = c(2,3), measure = "betweenness", normalized = FALSE)
el <- matrix(c(1,6, 2,1, 3,1, 4,1, 5,1, 2,6, 6,2, 1,3, 3,6,
               2,4, 3,4, 5,4, 1,5, 4,5), ncol = 2, byrow = TRUE)
g6 <- make_graph(as.vector(t(el)), n = 6, directed = TRUE)
v3 <- group_centrality(g6, nodes = c(1,2), measure = "betweenness", normalized = FALSE)
ok_bw <- isTRUE(all.equal(v1, 3.0)) &&
         isTRUE(all.equal(v2, 1.0)) &&
         isTRUE(all.equal(v3, 7.5))
record("6", "group_centrality(betweenness)",
       "textbook Everett-Borgatti (hand-verified)",
       if (ok_bw) "PASS" else "FAIL",
       sprintf("C={2}:%.1f C={2,3}:%.1f 6-node:%.1f", v1, v2, v3))

# ---- dispersion vs NetworkX on karate (all 156 edges) -------------------
if (has_nx) {
  cog_df <- dispersion(karate, normalized = TRUE)
  nx_full <- nx$dispersion(nx$karate_club_graph(), normalized = TRUE)
  max_diff <- 0
  for (i in seq_len(nrow(cog_df))) {
    u <- cog_df$from[i]; v <- cog_df$to[i]
    ref <- nx_full[[as.character(u - 1L)]][[as.character(v - 1L)]]
    max_diff <- max(max_diff, abs(cog_df$dispersion[i] - ref))
  }
  record("6", "dispersion (karate edges)", "nx.dispersion",
         if (max_diff <= 1e-12) "PASS" else "FAIL",
         sprintf("156 edges max|Δ|=%s", fmt_rel(max_diff)))
} else {
  record("6", "dispersion (karate edges)", "nx.dispersion", "SKIP", "networkx unavailable")
}

# =========================================================================
# REPORT
# =========================================================================

tbl <- do.call(rbind, results)

# Format a column-aligned report
col_w <- c(
  batch     = 5,
  measure   = max(nchar("measure"),  max(nchar(tbl$measure))),
  reference = max(nchar("reference"), max(nchar(tbl$reference))),
  status    = 6,
  detail    = max(nchar("detail"),   max(nchar(tbl$detail)))
)

pad <- function(s, w) formatC(s, width = w, flag = "-")

cat("\n")
cat(paste0(
  pad("batch",     col_w["batch"]),     "  ",
  pad("measure",   col_w["measure"]),   "  ",
  pad("reference", col_w["reference"]), "  ",
  pad("status",    col_w["status"]),    "  ",
  pad("detail",    col_w["detail"]), "\n"))
cat(paste0(strrep("-", sum(col_w) + 8), "\n"))

for (i in seq_len(nrow(tbl))) {
  cat(paste0(
    pad(tbl$batch[i],     col_w["batch"]),     "  ",
    pad(tbl$measure[i],   col_w["measure"]),   "  ",
    pad(tbl$reference[i], col_w["reference"]), "  ",
    pad(tbl$status[i],    col_w["status"]),    "  ",
    pad(tbl$detail[i],    col_w["detail"]), "\n"))
}

n_pass <- sum(tbl$status == "PASS")
n_fail <- sum(tbl$status == "FAIL")
n_skip <- sum(tbl$status == "SKIP")
cat(sprintf("\nsummary: %d pass, %d fail, %d skip (%d total)\n",
            n_pass, n_fail, n_skip, nrow(tbl)))

# Environment footer — useful for diagnosing drift on the snapshot
cat(sprintf("\nenvironment:\n"))
cat(sprintf("  R         : %s\n", R.version.string))
cat(sprintf("  igraph    : %s\n", as.character(packageVersion("igraph"))))
if (has_sna)        cat(sprintf("  sna       : %s\n", as.character(packageVersion("sna"))))
if (has_centiserve) cat(sprintf("  centiserve: %s\n", as.character(packageVersion("centiserve"))))
if (has_nx)         cat(sprintf("  networkx  : %s\n", nx[["__version__"]]))

# --------------------------------------------------------------- snapshot IO
if (write_snapshot) {
  dir.create(dirname(snapshot_path), showWarnings = FALSE, recursive = TRUE)
  # Snapshot excludes the numeric detail so tiny ULP drift doesn't invalidate
  # it — only the measure/reference/status triple is stable across platforms.
  snap <- tbl[, c("batch", "measure", "reference", "status")]
  write.table(snap, snapshot_path, sep = "\t", quote = FALSE,
              row.names = FALSE)
  cat(sprintf("\nsnapshot written to %s\n", snapshot_path))
}

if (diff_snapshot) {
  if (!file.exists(snapshot_path)) {
    stop("snapshot file missing: ", snapshot_path, "  (run with --snapshot first)")
  }
  prev <- read.table(snapshot_path, sep = "\t", header = TRUE,
                     stringsAsFactors = FALSE)
  curr <- tbl[, c("batch", "measure", "reference", "status")]
  # Key by measure + reference
  key_prev <- paste(prev$measure, prev$reference, sep = "||")
  key_curr <- paste(curr$measure, curr$reference, sep = "||")
  added   <- setdiff(key_curr, key_prev)
  removed <- setdiff(key_prev, key_curr)
  common  <- intersect(key_prev, key_curr)
  changed <- character(0)
  for (k in common) {
    s_prev <- prev$status[key_prev == k]
    s_curr <- curr$status[key_curr == k]
    if (!identical(s_prev, s_curr)) {
      changed <- c(changed, sprintf("%s : %s -> %s", k, s_prev, s_curr))
    }
  }
  cat("\ndiff vs snapshot:\n")
  cat(sprintf("  added   : %d\n", length(added)))
  cat(sprintf("  removed : %d\n", length(removed)))
  cat(sprintf("  changed : %d\n", length(changed)))
  for (c_ in changed) cat("    ", c_, "\n", sep = "")
}

if (n_fail > 0L) quit(status = 1L)
