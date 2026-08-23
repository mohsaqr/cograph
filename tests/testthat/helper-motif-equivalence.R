# Shared generators and reference checks for the motif Monte Carlo
# equivalence suite (test-motifs-equivalence-mc.R) and the local 1000-dataset
# sweep. Every dataset is deterministic in its seed.

.motif_mc_man_names <- c("003", "012", "102", "021D", "021U", "021C",
                         "111D", "111U", "030T", "030C", "201",
                         "120D", "120U", "120C", "210", "300")

# ---- topology catalogue ----------------------------------------------------

# Returns an igraph of the requested shape. Shapes cover the standard random
# families plus deterministic extremes and pathological cases (disconnected,
# isolates, empty, reciprocity-heavy, DAG, bipartite blocks, tournament).
.motif_mc_graph <- function(shape, n, directed) {
  g <- switch(
    shape,
    gnp_sparse = igraph::sample_gnp(n, min(1, 1.5 / n), directed = directed),
    gnp_medium = igraph::sample_gnp(n, 0.2, directed = directed),
    gnp_dense = igraph::sample_gnp(n, 0.6, directed = directed),
    gnm = igraph::sample_gnm(n, sample.int(max(1, n * (n - 1) / 3), 1),
                             directed = directed),
    scale_free = igraph::sample_pa(n, power = 1, m = 2,
                                   directed = directed),
    small_world = {
      g0 <- igraph::sample_smallworld(1, n, min(3, max(1, n %/% 4)), 0.1)
      if (directed) .motif_mc_orient(g0) else g0
    },
    ring = igraph::make_ring(n, directed = directed),
    star_out = igraph::make_star(n, mode = if (directed) "out" else
      "undirected"),
    star_in = igraph::make_star(n, mode = if (directed) "in" else
      "undirected"),
    complete = igraph::make_full_graph(n, directed = directed),
    tournament = {
      g0 <- igraph::make_full_graph(n, directed = FALSE)
      .motif_mc_orient(g0)
    },
    dag = {
      m <- matrix(0, n, n)
      m[upper.tri(m)] <- stats::rbinom(n * (n - 1) / 2, 1, 0.3)
      igraph::graph_from_adjacency_matrix(m, mode = "directed")
    },
    two_blocks = {
      m <- matrix(0, n, n)
      half <- n %/% 2
      a <- seq_len(half); b <- setdiff(seq_len(n), a)
      m[a, b] <- stats::rbinom(length(a) * length(b), 1, 0.4)
      if (!directed) m <- pmax(m, t(m))
      igraph::graph_from_adjacency_matrix(
        m, mode = if (directed) "directed" else "undirected")
    },
    disconnected = {
      k <- sample(2:3, 1)
      parts <- lapply(seq_len(k), function(i) {
        ni <- max(3, n %/% k)
        igraph::sample_gnp(ni, 0.4, directed = directed)
      })
      do.call(igraph::disjoint_union, parts)
    },
    isolates = {
      g0 <- igraph::sample_gnp(max(3, n - 3), 0.3, directed = directed)
      igraph::add_vertices(g0, 3)
    },
    reciprocal = {
      m <- matrix(stats::rbinom(n * n, 1, 0.25), n, n)
      m <- pmax(m, t(m)); diag(m) <- 0
      igraph::graph_from_adjacency_matrix(
        m, mode = if (directed) "directed" else "undirected")
    },
    empty = igraph::make_empty_graph(n, directed = directed),
    stop("unknown shape: ", shape)
  )
  igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
}

# Randomly orient an undirected graph (for tournaments / directed
# small-world): each edge gets one direction, some get both.
.motif_mc_orient <- function(g) {
  el <- igraph::as_edgelist(g, names = FALSE)
  if (nrow(el) == 0) {
    return(igraph::make_empty_graph(igraph::vcount(g), directed = TRUE))
  }
  flip <- stats::runif(nrow(el)) < 0.5
  el[flip, ] <- el[flip, 2:1]
  both <- stats::runif(nrow(el)) < 0.2
  el2 <- rbind(el, el[both, 2:1, drop = FALSE])
  igraph::graph_from_edgelist(el2, directed = TRUE) |>
    igraph::add_vertices(max(0, igraph::vcount(g) - max(el2)))
}

.motif_mc_shapes <- c("gnp_sparse", "gnp_medium", "gnp_dense", "gnm",
                      "scale_free", "small_world", "ring", "star_out",
                      "star_in", "complete", "tournament", "dag",
                      "two_blocks", "disconnected", "isolates",
                      "reciprocal", "empty")

# ---- dataset generator -----------------------------------------------------

# Dataset categories cycle deterministically with the index:
#   directed / weighted-directed / undirected / multi-actor edge list.
# Sizes span 3..60 nodes; edge lists span 2..30 actors over 3..8 states.
make_motif_mc_dataset <- function(i) {
  set.seed(10000 + i)
  category <- switch((i %% 5) + 1L,
                     "directed", "directed", "weighted", "undirected",
                     "edgelist")
  if (category == "edgelist") {
    n_actors <- sample(2:30, 1)
    n_states <- sample(3:8, 1)
    states <- LETTERS[seq_len(n_states)]
    rows_per <- sample(3:25, n_actors, replace = TRUE)
    el <- do.call(rbind, lapply(seq_len(n_actors), function(a) {
      data.frame(actor = sprintf("a%02d", a),
                 from = sample(states, rows_per[a], replace = TRUE),
                 to = sample(states, rows_per[a], replace = TRUE),
                 stringsAsFactors = FALSE)
    }))
    el <- el[el$from != el$to, , drop = FALSE]
    if (nrow(el) < 3) {
      el <- data.frame(actor = "a01", from = states[1:2], to = states[2:3],
                       stringsAsFactors = FALSE)
    }
    return(list(category = category, shape = "edgelist", el = el,
                states = states, seed = 10000 + i))
  }

  directed <- category != "undirected"
  # undirected extremes with directed-only shapes swapped out
  shapes <- if (directed) .motif_mc_shapes else
    setdiff(.motif_mc_shapes, c("tournament", "dag"))
  shape <- sample(shapes, 1)
  n <- sample(c(3:8, 10, 12, 15, 20, 30, 40, 60), 1)
  if (!directed) n <- min(n, 25) # keeps brute-force reference feasible
  g <- .motif_mc_graph(shape, n, directed)
  mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
  if (category == "weighted") {
    nz <- which(mat > 0)
    w <- sample(c(stats::rpois(length(nz), 3) + 1,
                  round(stats::runif(length(nz), 0.1, 2.5), 2)),
                length(nz))
    mat[nz] <- w[seq_along(nz)]
  }
  dimnames(mat) <- list(paste0("V", seq_len(nrow(mat))),
                        paste0("V", seq_len(nrow(mat))))
  list(category = category, shape = shape, mat = mat, directed = directed,
       n = nrow(mat), seed = 10000 + i)
}

# ---- reference checks ------------------------------------------------------

# Brute-force induced undirected 3-node classes (independent of the package
# formula): classify every triple by its edge count.
.motif_mc_brute_undirected <- function(mat) {
  bin <- (mat > 0) * 1L
  n <- nrow(bin)
  if (n < 3) return(c(empty = 0, edge = 0, wedge = 0, triangle = 0))
  combos <- utils::combn(n, 3)
  e <- bin[cbind(combos[1, ], combos[2, ])] +
    bin[cbind(combos[1, ], combos[3, ])] +
    bin[cbind(combos[2, ], combos[3, ])]
  c(empty = sum(e == 0), edge = sum(e == 1),
    wedge = sum(e == 2), triangle = sum(e == 3))
}

# Run every applicable equivalence check for one dataset; returns a character
# vector of failure descriptions (empty = fully equivalent).
check_motif_mc_dataset <- function(ds) {
  fails <- character(0)
  add <- function(msg) fails <<- c(fails, sprintf("[%s/%s seed=%d] %s",
                                                  ds$category, ds$shape,
                                                  ds$seed, msg))

  if (ds$category == "edgelist") {
    # Independent reference: per-actor binary transition matrix -> igraph
    # triad_census, summed over actors.
    states <- ds$states
    ref <- stats::setNames(numeric(16), .motif_mc_man_names)
    for (a in unique(ds$el$actor)) {
      sub <- ds$el[ds$el$actor == a, ]
      m <- matrix(0L, length(states), length(states),
                  dimnames = list(states, states))
      for (r in seq_len(nrow(sub))) m[sub$from[r], sub$to[r]] <- 1L
      tc <- igraph::triad_census(
        igraph::graph_from_adjacency_matrix(m, mode = "directed"))
      ref <- ref + as.numeric(tc)
    }
    cen <- suppressMessages(
      motifs(ds$el, pattern = "all", significance = FALSE,
             min_transitions = 0, min_count = NULL))
    got <- stats::setNames(numeric(16), .motif_mc_man_names)
    got[cen$results$type] <- cen$results$count
    if (!isTRUE(all.equal(unname(got), unname(ref)))) {
      add("census vs per-actor igraph triad_census mismatch")
    }
    ins <- suppressMessages(
      motifs(ds$el, named_nodes = TRUE, pattern = "all",
             significance = FALSE, min_transitions = 0, min_count = NULL))
    ins_tot <- tapply(ins$results$observed, ins$results$type, sum)
    for (nm in names(ins_tot)) {
      if (!isTRUE(all.equal(as.numeric(ins_tot[[nm]]),
                            as.numeric(got[[nm]])))) {
        add(sprintf("instance total disagrees with census for %s", nm))
      }
    }

    # The legacy extractor must retain the same per-(triple, type) totals.
    legacy <- suppressWarnings(
      extract_motifs(data = ds$el, id = "actor", pattern = "all",
                     significance = FALSE, min_transitions = 0))
    if (!is.null(legacy)) {
      legacy_tot <- tapply(legacy$results$observed,
                           legacy$results$type, sum)
      legacy_got <- stats::setNames(numeric(16), .motif_mc_man_names)
      legacy_got[names(legacy_tot)] <- legacy_tot
      if (!isTRUE(all.equal(unname(legacy_got), unname(got)))) {
        add("extract_motifs per-type totals disagree with census")
      }
    }
    return(fails)
  }

  mat <- ds$mat
  bin <- (mat > 0) * 1L

  if (ds$directed) {
    g <- igraph::graph_from_adjacency_matrix(bin, mode = "directed")
    ref <- as.integer(igraph::triad_census(g))
    if (sum(ref) != choose(nrow(bin), 3)) {
      add("igraph census does not sum to choose(n,3) [reference broken]")
    }

    ct <- triad_census(mat)
    if (!identical(as.integer(ct), ref)) add("triad_census mismatch")

    mc <- motif_census(mat, directed = TRUE, n_random = 2,
                       seed = ds$seed)
    got <- stats::setNames(as.integer(mc$count), mc$motif)
    if (!identical(unname(got[.motif_mc_man_names]), ref)) {
      add("motif_census counts/labels mismatch")
    }

    r <- suppressMessages(
      motifs(mat, pattern = "all", significance = FALSE,
             min_transitions = 0, min_count = NULL))
    got2 <- stats::setNames(numeric(16), .motif_mc_man_names)
    got2[r$results$type] <- r$results$count
    if (!isTRUE(all.equal(unname(got2), as.numeric(ref)))) {
      add("motifs(pattern='all') census mismatch")
    }

    # configuration null must preserve the exact degree sequence
    g_rand <- cograph:::.generate_random_graph(g, "configuration")
    if (!identical(igraph::degree(g, mode = "in"),
                   igraph::degree(g_rand, mode = "in")) ||
        !identical(igraph::degree(g, mode = "out"),
                   igraph::degree(g_rand, mode = "out"))) {
      add("configuration null changed the degree sequence")
    }
  } else {
    ref <- .motif_mc_brute_undirected(mat)
    mc <- motif_census(mat, directed = FALSE, method = "gnm",
                       n_random = 2, seed = ds$seed)
    got <- stats::setNames(mc$count, mc$motif)
    if (!isTRUE(all.equal(unname(got[names(ref)]), unname(as.numeric(ref))))) {
      add("undirected census disagrees with brute-force enumeration")
    }
    g <- igraph::graph_from_adjacency_matrix(bin, mode = "undirected")
    g_rand <- cograph:::.generate_random_graph(g, "configuration")
    if (!identical(igraph::degree(g), igraph::degree(g_rand))) {
      add("undirected configuration null changed the degree sequence")
    }
  }
  fails
}
