# Reference-side helpers for the community, cluster, conversion, multilayer and weight files (slice D2), in
# base R. Nothing here calls cograph: each function builds an input both sides read, writes an algorithm out
# from its published definition, or shapes a reference's result.

#' Canonical community labels: each node labelled by the first node (in `nodes` order) of its community
#'
#' Two partitions are the same partition exactly when their canonical labels are equal node by node, whatever
#' numbers the two programs gave the communities.
#'
#' @param membership A vector of community labels named by node.
#' @param nodes The node order that decides which member names a community.
#' @return A character vector named by node.
ref_canonical <- function(membership, nodes = names(membership)) {
  stopifnot("`membership` must be named by node" = !is.null(names(membership)) && all(nodes %in% names(membership)))
  membership <- membership[nodes]
  stats::setNames(nodes[match(membership, membership)], nodes)
}

#' A Python list of strings, e.g. `['N1', 'N2']`
#'
#' @param values A character vector.
#' @return One string, a Python list literal.
py_strings <- function(values) {
  sprintf("[%s]", paste0("'", values, "'", collapse = ", "))
}

#' A partition as a Python list of sets of node names, the form NetworkX's community functions take
#'
#' @param membership A vector of community labels named by node.
#' @return One string, e.g. `[{'N1', 'N2'}, {'N3'}]`.
py_partition <- function(membership) {
  groups <- split(names(membership), membership)
  sprintf("[%s]", paste(vapply(groups, \(members) sprintf("{%s}", paste0("'", members, "'", collapse = ", ")),
                               character(1)), collapse = ", "))
}

#' A matrix from the flat row-major values a Python expression returned
#'
#' @param values Numeric vector of length `length(nodes)^2`, row by row.
#' @param nodes Row and column names.
#' @return A square matrix with dimnames `nodes`.
ref_square <- function(values, nodes) {
  stopifnot("one value per cell is needed" = length(values) == length(nodes)^2)
  matrix(as.numeric(values), length(nodes), length(nodes), byrow = TRUE, dimnames = list(nodes, nodes))
}

#' An edge set as a table keyed by edge, for comparing which edges two programs keep
#'
#' @param from,to Endpoint names.
#' @param directed Whether `from -> to` differs from `to -> from`; when not, each edge is written with its
#'   endpoints in sorted order.
#' @return A data frame with the key column `edge` and the column `kept` (TRUE).
ref_edge_set <- function(from, to, directed = FALSE) {
  from <- as.character(from)
  to <- as.character(to)
  first <- if (directed) from else ifelse(from < to, from, to)
  second <- if (directed) to else ifelse(from < to, to, from)
  data.frame(edge = paste(first, second, sep = if (directed) " -> " else " -- "), kept = rep(TRUE, length(from)))
}

#' Which edges of a graph a selection kept, one row per edge of the graph
#'
#' @param graph The adjacency matrix the selection was made on.
#' @param kept_from,kept_to Endpoint names of the kept edges.
#' @param directed Whether the edges are arcs.
#' @return A data frame with the key column `edge` and the logical column `kept`.
ref_edge_flags <- function(graph, kept_from, kept_to, directed = !isSymmetric(unname(graph))) {
  cells <- which(graph != 0, arr.ind = TRUE)
  if (!directed) cells <- cells[cells[, 1L] < cells[, 2L], , drop = FALSE]
  all_edges <- ref_edge_set(rownames(graph)[cells[, 1L]], colnames(graph)[cells[, 2L]], directed)$edge
  kept <- ref_edge_set(kept_from, kept_to, directed)$edge
  data.frame(edge = all_edges, kept = all_edges %in% kept)
}

#' Which nodes of a graph a selection kept, one entry per node
#'
#' @param nodes All node names.
#' @param kept The names of the kept nodes.
#' @return A logical vector named by node.
ref_node_flags <- function(nodes, kept) {
  stats::setNames(nodes %in% kept, nodes)
}

#' A weight matrix rebuilt from an edge list
#'
#' @param from,to Endpoints, as integer positions or names.
#' @param weight Edge weights.
#' @param nodes Node names, in matrix order.
#' @param directed Whether each edge is one arc; when not, each is written into both triangles.
#' @return A square matrix with dimnames `nodes`.
ref_edgelist_matrix <- function(from, to, weight, nodes, directed) {
  graph <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  cells <- cbind(if (is.character(from)) match(from, nodes) else from, if (is.character(to)) match(to, nodes) else to)
  graph[cells] <- weight
  if (!directed) graph[cells[, 2:1, drop = FALSE]] <- weight
  graph
}

#' The modularity matrix B = A - k k' / 2m of an undirected weighted graph (Newman 2006)
#'
#' @param graph A symmetric weight matrix.
#' @return The matrix B, with the dimnames of `graph`.
ref_modularity_matrix <- function(graph) {
  degrees <- rowSums(graph)
  graph - outer(degrees, degrees) / sum(graph)
}

#' Modularity of a partition from Newman's definition, Q = (1/2m) sum_ij B_ij [c_i = c_j]
#'
#' For a directed graph, Leicht and Newman (2008): Q = (1/m) sum_ij (A_ij - k_i^out k_j^in / m) [c_i = c_j].
#'
#' @param graph A weight matrix.
#' @param membership Community labels named by node.
#' @param directed Whether to use the directed form.
#' @return One number.
ref_modularity <- function(graph, membership, directed = FALSE) {
  membership <- membership[rownames(graph)]
  total <- sum(graph)
  expected <- if (directed) outer(rowSums(graph), colSums(graph)) / total else outer(rowSums(graph), rowSums(graph)) / total
  sum((graph - expected) * outer(membership, membership, `==`)) / total
}

#' Newman's (2006) leading-eigenvector method: repeated division by the leading eigenvector
#'
#' A group g is divided by the signs of the leading eigenvector of its generalised modularity matrix
#' B(g)_ij = B_ij - delta_ij sum_{k in g} B_ik, as long as the leading eigenvalue is positive and the division
#' raises modularity (s' B(g) s > 0); otherwise g is a community.
#'
#' @param graph A symmetric weight matrix with dimnames.
#' @return Community numbers named by node.
ref_leading_eigenvector <- function(graph) {
  modularity <- ref_modularity_matrix(graph)
  divide <- function(members) {
    if (length(members) < 2L) return(list(members))
    block <- modularity[members, members, drop = FALSE]
    generalised <- block - diag(rowSums(block), length(members))
    leading <- eigen(generalised, symmetric = TRUE)
    side <- leading$vectors[, 1L] > 0
    signs <- ifelse(side, 1, -1)
    gain <- drop(crossprod(signs, generalised %*% signs))
    if (leading$values[[1L]] <= 1e-8 || gain <= 1e-10 || all(side) || !any(side)) return(list(members))
    c(divide(members[side]), divide(members[!side]))
  }
  groups <- divide(rownames(graph))
  stats::setNames(rep(seq_along(groups), lengths(groups)), unlist(groups))[rownames(graph)]
}

#' Pons and Latapy's (2005) walktrap: agglomeration by the random-walk distance, cut at maximum modularity
#'
#' Every node gets a loop of weight equal to its mean edge weight (the convention of the authors' program),
#' P = D^-1 A is raised to `steps`, and at each step the two adjacent communities with the smallest
#' delta sigma = (1/n) |C1||C2| / (|C1| + |C2|) * sum_k (P^t_C1k - P^t_C2k)^2 / d(k) are merged, the merged
#' profile being the size-weighted mean. The partition returned is the one of highest modularity (on the graph
#' without the added loops) along the merges.
#'
#' @param graph A symmetric weight matrix with dimnames, connected.
#' @param steps The length of the random walks.
#' @return A list: `membership` (community numbers named by node) and `modularity`.
ref_walktrap <- function(graph, steps = 4L) {
  nodes <- rownames(graph)
  n <- length(nodes)
  neighbours <- rowSums(graph != 0)
  looped <- graph + diag(ifelse(neighbours > 0, rowSums(graph) / pmax(neighbours, 1), 1), n)
  strength <- rowSums(looped)
  transition <- looped / strength
  walk <- Reduce(`%*%`, replicate(steps, transition, simplify = FALSE))
  modularity <- ref_modularity_matrix(graph) / sum(graph)
  score <- \(groups) sum(vapply(groups, \(members) sum(modularity[members, members]), numeric(1)))
  start <- list(groups = as.list(seq_len(n)), profile = walk, history = list(as.list(seq_len(n))))
  merged <- Reduce(\(state, step) {
    groups <- state$groups
    sizes <- lengths(groups)
    pairs <- t(utils::combn(length(groups), 2L))
    adjacent <- vapply(seq_len(nrow(pairs)), \(p) sum(graph[groups[[pairs[p, 1L]]], groups[[pairs[p, 2L]]]]) > 0,
                       logical(1))
    pairs <- pairs[adjacent, , drop = FALSE]
    cost <- vapply(seq_len(nrow(pairs)), \(p) {
      a <- pairs[p, 1L]
      b <- pairs[p, 2L]
      sizes[a] * sizes[b] / (sizes[a] + sizes[b]) * sum((state$profile[a, ] - state$profile[b, ])^2 / strength) / n
    }, numeric(1))
    best <- pairs[which.min(cost), ]
    a <- best[[1L]]
    b <- best[[2L]]
    profile <- state$profile
    profile[a, ] <- (sizes[a] * profile[a, ] + sizes[b] * profile[b, ]) / (sizes[a] + sizes[b])
    groups[[a]] <- c(groups[[a]], groups[[b]])
    list(groups = groups[-b], profile = profile[-b, , drop = FALSE], history = c(state$history, list(groups[-b])))
  }, seq_len(n - 1L), start)
  scores <- vapply(merged$history, score, numeric(1))
  best <- merged$history[[which.max(scores)]]
  membership <- integer(n)
  membership[unlist(best)] <- rep(seq_along(best), lengths(best))
  list(membership = stats::setNames(membership, nodes), modularity = max(scores))
}

#' Every partition of `n` items, as restricted growth strings (one row per partition)
#'
#' @param n Number of items (the Bell number B(n) of rows: 21147 for 9).
#' @return An integer matrix with `n` columns.
ref_set_partitions <- function(n) {
  Reduce(\(partial, position) {
    grown <- lapply(seq_len(nrow(partial)), \(row) {
      labels <- seq_len(max(partial[row, ]) + 1L)
      cbind(partial[rep(row, length(labels)), , drop = FALSE], labels, deparse.level = 0L)
    })
    do.call(rbind, grown)
  }, seq_len(n - 1L), matrix(1L, 1L, 1L))
}

#' The partition of maximum modularity, by enumerating every partition of the nodes
#'
#' @param graph A symmetric weight matrix with dimnames, of at most 10 nodes.
#' @return A list: `membership` (named by node) and `modularity`.
ref_optimal_modularity <- function(graph) {
  stopifnot("enumeration is for at most 10 nodes" = nrow(graph) <= 10L)
  modularity <- ref_modularity_matrix(graph) / sum(graph)
  partitions <- ref_set_partitions(nrow(graph))
  pairs <- which(upper.tri(modularity, diag = TRUE), arr.ind = TRUE)
  # each unordered pair i < j appears twice in the double sum
  pair_weight <- modularity[pairs] * ifelse(pairs[, 1L] == pairs[, 2L], 1, 2)
  together <- vapply(seq_len(nrow(pairs)), \(p) partitions[, pairs[p, 1L]] == partitions[, pairs[p, 2L]],
                     logical(nrow(partitions)))
  scores <- as.vector(together %*% pair_weight)
  best <- which.max(scores)
  list(membership = stats::setNames(partitions[best, ], rownames(graph)), modularity = scores[[best]])
}

#' Distances and similarities between two partitions, from the contingency table
#'
#' Variation of information (Meila 2007, natural logarithm), normalised mutual information 2 I / (H1 + H2)
#' (Danon et al. 2005), split-join distance (van Dongen 2000: the two projection distances summed), Rand index
#' (Rand 1971) and adjusted Rand index (Hubert and Arabie 1985).
#'
#' @param first,second Community labels of the same nodes, in the same order.
#' @return A named numeric vector: `vi`, `nmi`, `split.join`, `rand`, `adjusted.rand`.
ref_partition_scores <- function(first, second) {
  counts <- table(first, second)
  n <- sum(counts)
  joint <- counts / n
  row_p <- rowSums(joint)
  col_p <- colSums(joint)
  occupied <- counts > 0
  entropy <- \(p) -sum(p[p > 0] * log(p[p > 0]))
  mutual <- sum(joint[occupied] * log(joint[occupied] / outer(row_p, col_p)[occupied]))
  pairs <- \(v) sum(v * (v - 1) / 2)
  same_both <- pairs(counts)
  same_first <- pairs(rowSums(counts))
  same_second <- pairs(colSums(counts))
  total <- n * (n - 1) / 2
  expected <- same_first * same_second / total
  c(vi = entropy(row_p) + entropy(col_p) - 2 * mutual,
    nmi = 2 * mutual / (entropy(row_p) + entropy(col_p)),
    split.join = (n - sum(apply(counts, 1L, max))) + (n - sum(apply(counts, 2L, max))),
    rand = (total + 2 * same_both - same_first - same_second) / total,
    adjusted.rand = (same_both - expected) / ((same_first + same_second) / 2 - expected))
}

#' The disparity filter of Serrano, Boguna and Vespignani (2009), written from the paper
#'
#' For an edge of node i with normalised weight p_ij = w_ij / s_i and degree k_i, alpha_ij = (1 - p_ij)^(k_i - 1)
#' (equation 2 of the paper integrated); a node of degree 1 gives alpha = 1. An edge is kept when alpha < level
#' for either endpoint. In a directed network the out-edges of i are tested against i's out-strength and
#' out-degree, the in-edges of j against j's in-strength and in-degree (the paper's directed extension).
#'
#' @param graph A non-negative weight matrix.
#' @param level The significance level.
#' @return A 0/1 integer matrix with the dimnames of `graph` and a zero diagonal.
ref_disparity <- function(graph, level) {
  present <- graph > 0
  alpha_of <- \(weights, strength, degree) {
    share <- weights / ifelse(strength > 0, strength, 1)
    ifelse(degree > 1, (1 - share)^(degree - 1), 1)
  }
  # out side: row i; in side: column j
  out_alpha <- alpha_of(graph, rowSums(graph)[row(graph)], rowSums(present)[row(graph)])
  in_alpha <- alpha_of(graph, colSums(graph)[col(graph)], colSums(present)[col(graph)])
  kept <- present & (out_alpha < level | in_alpha < level)
  diag(kept) <- FALSE
  matrix(as.integer(kept), nrow(graph), ncol(graph), dimnames = dimnames(graph))
}

#' Aggregate the cells of a weight matrix into a cluster-by-cluster table, from the definition
#'
#' Cells that are zero (no edge) are left out before aggregating, as an absent edge has no weight.
#'
#' @param graph A weight matrix with dimnames.
#' @param clusters A named list of node names.
#' @param aggregate A function of a numeric vector, or "density" (sum over the n_i n_j possible cells).
#' @return A square matrix, one row and column per cluster, zero where no edge runs between two clusters.
ref_cluster_table <- function(graph, clusters, aggregate) {
  names_of <- names(clusters)
  vapply(names_of, \(to) vapply(names_of, \(from) {
    block <- graph[clusters[[from]], clusters[[to]], drop = FALSE]
    weights <- block[block != 0]
    if (!length(weights)) return(0)
    if (identical(aggregate, "density")) sum(weights) / length(block) else aggregate(weights)
  }, numeric(1)), numeric(length(names_of)))
}

#' First-order transition probabilities and initial-state proportions counted from wide sequences
#'
#' Each pair of consecutive non-missing states in a row is one transition; probabilities are row proportions of
#' the counts. The initial proportion of a state is its share of the first column.
#'
#' @param sequences A data frame of character states, one row per sequence, one column per time point.
#' @return A list: `weights` (a square matrix over the sorted states) and `inits` (named by state).
ref_transitions <- function(sequences) {
  states <- sort(unique(stats::na.omit(unlist(sequences, use.names = FALSE))))
  from <- unlist(sequences[, -ncol(sequences)], use.names = FALSE)
  to <- unlist(sequences[, -1L], use.names = FALSE)
  observed <- !is.na(from) & !is.na(to)
  counts <- unclass(table(factor(from[observed], states), factor(to[observed], states)))
  dimnames(counts) <- list(states, states)
  first <- table(factor(sequences[[1L]], states))
  list(weights = counts / rowSums(counts), inits = stats::setNames(as.numeric(first / sum(first)), states))
}
