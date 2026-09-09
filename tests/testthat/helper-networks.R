# Test-network corpus: native descriptors and accessors.
#
# This file is sourced by testthat before every test file, and also by the
# corpus build scripts under local_testing_and_equivalence/networks/, so the
# manifest columns are computed by exactly one implementation. Nothing here
# uses igraph: the corpus must remain readable when igraph is absent.
#
# Design: docs/test-networks-plan.md.

.hash_matrix <- function(m) {
  digest::digest(serialize(m, NULL, version = 3), algo = "sha256", serialize = FALSE)
}

.components_native <- function(m) {
  # Connected components of the underlying undirected simple graph, by BFS
  # with a vectorised frontier. The while loop walks one component per
  # iteration; each frontier expansion is a matrix-vector product.
  n <- nrow(m)
  if (n == 0L) return(integer(0))
  b <- (m != 0) | (t(m) != 0)
  diag(b) <- FALSE
  storage.mode(b) <- "double"
  comp <- integer(n)
  k <- 0L
  while (any(comp == 0L)) {
    k <- k + 1L
    seed <- which(comp == 0L)[1L]
    reached <- logical(n)
    reached[seed] <- TRUE
    repeat {
      frontier <- as.vector(b %*% reached) > 0
      new <- frontier & !reached
      if (!any(new)) break
      reached <- reached | new
    }
    comp[reached] <- k
  }
  comp
}

.describe_matrix <- function(name, m, tier, family, source, license,
                             has_multi = FALSE, bipartite = FALSE,
                             labels_assigned = FALSE, seed = NA_integer_,
                             projection_of = NA_character_, zero_weight_edges = 0L,
                             dropped_na_rows = 0L) {
  stopifnot(
    "matrix must be numeric" = is.numeric(m),
    "matrix must be square" = nrow(m) == ncol(m),
    "matrix must be finite" = all(is.finite(m))
  )
  n <- nrow(m)
  directed <- n > 0L && !isSymmetric(unname(m))
  nz <- m != 0
  n_edges <- if (directed) sum(nz) else sum(nz[upper.tri(nz, diag = TRUE)])
  vals <- m[nz]
  comp <- .components_native(m)
  deg <- if (n > 0L) rowSums(nz) + colSums(nz) else integer(0)
  data.frame(
    name = name, tier = tier, family = family, source = source, license = license,
    n = n, m = n_edges, directed = directed,
    weighted = length(vals) > 0L && any(vals != 1),
    signed = any(vals < 0),
    has_loops = n > 0L && any(diag(nz)),
    has_multi = has_multi,
    n_components = if (n > 0L) max(comp) else 0L,
    n_isolates = sum(deg == 0),
    bipartite = bipartite,
    has_dimnames = !is.null(rownames(m)),
    labels_assigned = labels_assigned,
    zero_weight_edges = zero_weight_edges,
    dropped_na_rows = dropped_na_rows,
    min_weight = if (length(vals)) min(vals) else NA_real_,
    max_weight = if (length(vals)) max(vals) else NA_real_,
    seed = seed, projection_of = projection_of,
    stored = TRUE,
    sha256 = .hash_matrix(m),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Accessors used by tests
# ---------------------------------------------------------------------------

.corpus_tiers <- c("real_small", "degenerate", "real_large", "synthetic_scale", "icon")

.corpus_path <- function(tier) {
  committed <- testthat::test_path("networks", paste0(tier, ".rds"))
  if (file.exists(committed)) return(committed)
  local <- testthat::test_path("..", "..", "local_testing_and_equivalence", "networks",
                               paste0(tier, ".rds"))
  if (file.exists(local)) return(local)
  NA_character_
}

.read_corpus_tier <- function(tier) {
  path <- .corpus_path(tier)
  if (is.na(path)) return(NULL)
  readRDS(path)
}

#' Tidy manifest of test networks, one row per network, with a `matrix`
#' list-column. Local-only tiers yield zero rows when their RDS is absent.
test_networks <- function(tier = c("real_small", "degenerate", "real_large",
                                   "synthetic_scale", "icon", "all"),
                          directed = NULL, weighted = NULL, signed = NULL,
                          stored = TRUE, min_n = 0, max_n = Inf) {
  tier <- match.arg(tier, several.ok = TRUE)
  if ("all" %in% tier) tier <- .corpus_tiers
  parts <- lapply(tier, function(t) {
    obj <- .read_corpus_tier(t)
    if (is.null(obj)) return(NULL)
    man <- obj$manifest
    man$matrix <- unname(obj$matrices[man$name])
    man
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) {
    return(structure(data.frame(), corpus_missing = tier))
  }
  if (length(parts) > 1L) {
    # Tiers may carry extra catalogue columns (ICON does); combine on the
    # columns they share.
    common <- Reduce(intersect, lapply(parts, names))
    parts <- lapply(parts, function(p) p[, common, drop = FALSE])
  }
  man <- do.call(rbind, parts)
  keep <- man$n >= min_n & man$n <= max_n
  if (!is.null(directed)) keep <- keep & man$directed == directed
  if (!is.null(weighted)) keep <- keep & man$weighted == weighted
  if (!is.null(signed)) keep <- keep & man$signed == signed
  if (!is.null(stored)) keep <- keep & man$stored == stored
  out <- man[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' One test network by name, as a dense matrix.
test_network <- function(name) {
  hits <- lapply(.corpus_tiers, function(t) {
    obj <- .read_corpus_tier(t)
    if (!is.null(obj) && name %in% names(obj$matrices)) obj$matrices[[name]] else NULL
  })
  hits <- hits[!vapply(hits, is.null, logical(1))]
  if (!length(hits)) {
    stop(errorCondition(paste0("test network '", name, "' is not in any available tier"),
                        class = "cograph_test_network_missing", call = NULL))
  }
  hits[[1L]]
}

#' Skip the calling test when a local-only tier is not present.
skip_if_no_corpus_tier <- function(tier) {
  if (is.na(.corpus_path(tier))) {
    testthat::skip(paste0("test-network tier '", tier, "' not available (local only)"))
  }
}
