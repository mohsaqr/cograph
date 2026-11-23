# Reference computations for the local and structural centralities (slice A2): shortest distances, h-indices,
# core peelings and the reshaping of reference output, in base R. Nothing here calls cograph; each function is
# written from the published definition it names.

#' All shortest distances by Floyd-Warshall
#'
#' @param graph A square matrix with dimnames; a non-zero cell is an edge from row to column.
#' @param lengths Edge lengths, the same shape as `graph` (default: the entries of `graph`; pass `(graph != 0) * 1`
#'   for hop counts).
#' @return A square matrix of distances, `Inf` where no path exists, dimnames of `graph`.
ref_a2_distances <- function(graph, lengths = graph) {
  stopifnot("`graph` must be a square matrix" = is.matrix(graph) && nrow(graph) == ncol(graph))
  start <- ifelse(graph != 0, lengths, Inf)
  diag(start) <- 0
  # Floyd-Warshall: pass k allows paths through node k; each pass depends on the previous one.
  out <- Reduce(\(d, k) pmin(d, outer(d[, k], d[k, ], "+")), seq_len(nrow(graph)), start)
  dimnames(out) <- dimnames(graph)
  out
}

#' Hop distances: `ref_a2_distances()` with every edge of length one
#'
#' @param graph A square matrix with dimnames.
#' @return A square matrix of hop counts.
ref_a2_hops <- function(graph) ref_a2_distances(graph, (graph != 0) * 1)

#' The h-index of a set of numbers (Hirsch 2005): the largest h such that h of them are at least h
#'
#' @param values A numeric vector.
#' @return One number.
ref_a2_hindex <- function(values) {
  sorted <- sort(values, decreasing = TRUE)
  sum(sorted >= seq_along(sorted))
}

#' A node measure written as one function of the node index
#'
#' @param graph The adjacency matrix the measure is computed on.
#' @param fun A function of one node index returning one number.
#' @return A numeric vector named by node.
ref_a2_per_node <- function(graph, fun) {
  stats::setNames(vapply(seq_len(nrow(graph)), fun, numeric(1)), rownames(graph))
}

#' Peel a graph by a node score: the generic k-core / s-core recursion
#'
#' At each step every remaining node whose score on the remaining subgraph is at most the current level is
#' removed and labelled with that level; the level then rises to `next_level()` of the scores left (Batagelj and
#' Zaversnik 2003 for degree; Eidsaa and Almaas 2013 for strength; Garas et al. 2012 for integer thresholds).
#'
#' @param score A function of the logical vector of remaining nodes, returning one score per node.
#' @param n Number of nodes.
#' @param next_level A function of (current level, scores of the remaining nodes) giving the next level.
#' @param start The first level.
#' @return The level at which each node was removed.
ref_a2_peel <- function(score, n, next_level, start) {
  peel <- function(alive, label, level) {
    if (!any(alive)) return(label)
    current <- score(alive)
    drop <- alive & current <= level + 1e-12
    if (!any(drop)) return(peel(alive, label, next_level(level, current[alive])))
    label[drop] <- level
    peel(alive & !drop, label, level)
  }
  peel(rep(TRUE, n), numeric(n), start)
}

#' The s-core index of every node (Eidsaa and Almaas 2013)
#'
#' The s-core is the largest subgraph in which every node has strength at least s; a node's index is the largest
#' s whose core holds it. Peeling: remove every node at or below the smallest remaining strength, until none is,
#' then raise the level to the new smallest strength.
#'
#' @param weights A symmetric matrix of non-negative weights.
#' @return The s-core index per node, named by node.
ref_a2_s_core <- function(weights) {
  strength <- \(alive) colSums(weights * outer(alive, alive))
  stats::setNames(ref_a2_peel(strength, nrow(weights), \(level, left) min(left), min(colSums(weights))),
                  rownames(weights))
}

#' The k-core number by integer thresholds on a node score (k-core; Garas et al. 2012 for a generalised degree)
#'
#' @param score A function of the logical vector of remaining nodes.
#' @param graph The adjacency matrix (for the size and the names).
#' @return The integer threshold at which each node was removed, named by node.
ref_a2_integer_shells <- function(score, graph) {
  stats::setNames(ref_a2_peel(score, nrow(graph), \(level, left) level + 1, 0), rownames(graph))
}

#' Rows of (from, to, value) returned by NetworkX as a data frame keyed by node position
#'
#' @param triples A list of `list(from, to, value)` with node names.
#' @param graph The adjacency matrix (its row names give the positions).
#' @param value Name of the value column.
#' @return A data frame with integer `from`, `to` and the value column.
ref_a2_triples <- function(triples, graph, value) {
  out <- data.frame(from = match(vapply(triples, \(t) as.character(t[[1L]]), character(1)), rownames(graph)),
                    to = match(vapply(triples, \(t) as.character(t[[2L]]), character(1)), rownames(graph)))
  out[[value]] <- vapply(triples, \(t) as.numeric(t[[3L]]), numeric(1))
  out
}

#' A Python dict literal of node values, for a NetworkX call
#'
#' @param values A numeric vector named by node.
#' @return A string such as `{'N1': 0.1, 'N2': 0.5}`.
ref_a2_py_dict <- function(values) {
  sprintf("{%s}", paste(sprintf("'%s': %.17g", names(values), values), collapse = ", "))
}

# ---- node measures from their definitions (undirected simple graphs unless stated) ----------------------------

#' Binary adjacency of a matrix, diagonal cleared
#'
#' @param graph A square matrix.
#' @return A 0/1 matrix with the same dimnames.
ref_a2_binary <- function(graph) {
  out <- (graph != 0) * 1
  diag(out) <- 0
  out
}

#' The k-core number (Seidman 1983) by peeling at integer degree thresholds
#'
#' @param graph A symmetric adjacency matrix.
#' @return The core number per node, named by node.
ref_a2_kcore <- function(graph) {
  adjacency <- ref_a2_binary(graph)
  ref_a2_integer_shells(\(alive) rowSums(adjacency * outer(alive, alive)), graph)
}

#' Collective influence (Morone and Makse 2015): (k_i - 1) times the sum of (k_j - 1) over the nodes j at
#' distance exactly `radius`
#'
#' @param graph A symmetric adjacency matrix.
#' @param radius The radius of the ball.
#' @return One value per node.
ref_a2_collective_influence <- function(graph, radius = 2) {
  degree <- rowSums(ref_a2_binary(graph))
  boundary <- (ref_a2_hops(graph) == radius) * 1
  stats::setNames(as.numeric((degree - 1) * (boundary %*% (degree - 1))), rownames(graph))
}

#' Gravity centrality (Ma et al. 2016): sum over the nodes j within `radius` hops of k_s(i) k_s(j) / d_ij^2,
#' with k_s the k-core number
#'
#' @param graph A symmetric adjacency matrix.
#' @param radius Largest distance counted (`Inf` for all).
#' @param mass_i,mass_j The masses of the focal node and of its partners (default: the k-core number).
#' @return One value per node.
ref_a2_gravity <- function(graph, radius = 3, mass_i = ref_a2_kcore(graph), mass_j = mass_i) {
  hops <- ref_a2_hops(graph)
  ref_a2_per_node(graph, \(i) {
    within <- seq_len(nrow(graph)) != i & is.finite(hops[i, ]) & hops[i, ] <= radius
    sum(mass_i[[i]] * mass_j[within] / hops[i, within]^2)
  })
}

#' Local efficiency (Latora and Marchiori 2001): the mean of 1 / d_jk over the ordered pairs of neighbours of i,
#' distances measured in the subgraph of the neighbours alone; 0 for fewer than two neighbours
#'
#' @param graph A symmetric matrix of edge lengths (1 for hop counts).
#' @return One value per node.
ref_a2_local_efficiency <- function(graph) {
  ref_a2_per_node(graph, \(i) {
    neighbours <- setdiff(which(graph[i, ] != 0), i)
    if (length(neighbours) < 2L) return(0)
    d <- ref_a2_distances(graph[neighbours, neighbours, drop = FALSE])
    off <- row(d) != col(d)
    sum(1 / d[off]) / (length(neighbours) * (length(neighbours) - 1))
  })
}

#' Strength h-index: the h-index of the strengths of the node's neighbours (the lobby index of Korn et al. 2009,
#' Definition 1, with strength in place of degree; the node itself is not among them)
#'
#' @param graph A symmetric weight matrix.
#' @return One value per node.
ref_a2_hindex_strength <- function(graph) {
  strength <- rowSums(graph * (row(graph) != col(graph)))
  ref_a2_per_node(graph, \(i) ref_a2_hindex(strength[setdiff(which(graph[i, ] != 0), i)]))
}

#' Weighted h-index (Gao et al. 2019, eq. 3): the h-index of the multiset in which each neighbour j contributes
#' the link weight k_i k_j, repeated k_j times
#'
#' @param graph A symmetric adjacency matrix.
#' @return One value per node.
ref_a2_weighted_h_index <- function(graph) {
  adjacency <- ref_a2_binary(graph)
  degree <- rowSums(adjacency)
  ref_a2_per_node(graph, \(i) {
    neighbours <- which(adjacency[i, ] != 0)
    ref_a2_hindex(rep(degree[[i]] * degree[neighbours], times = degree[neighbours]))
  })
}

#' Redundancy (Burt 1992, as read by Borgatti 1997): the mean number of ties an alter has to the other alters,
#' 2 t / k with t the ties among the k neighbours
#'
#' @param graph A symmetric adjacency matrix.
#' @return One value per node.
ref_a2_redundancy <- function(graph) {
  adjacency <- ref_a2_binary(graph)
  ref_a2_per_node(graph, \(i) {
    neighbours <- which(adjacency[i, ] != 0)
    if (length(neighbours) < 2L) return(0)
    sum(adjacency[neighbours, neighbours]) / length(neighbours)
  })
}

#' Bridging coefficient (Hwang et al. 2008): (1 / k_i) / sum over the neighbours j of 1 / k_j
#'
#' @param graph A symmetric adjacency matrix.
#' @return One value per node.
ref_a2_bridging_coefficient <- function(graph) {
  adjacency <- ref_a2_binary(graph)
  degree <- rowSums(adjacency)
  stats::setNames((1 / degree) / as.numeric(adjacency %*% (1 / degree)), rownames(graph))
}

#' Renewed coreness (Liu, Tang, Zhou and Do 2015): drop every link whose diffusion importance
#' D_ij = (|N(j) \ N[i]| + |N(i) \ N[j]|) / 2 is below `threshold`, then take the k-core number of what is left
#'
#' @param graph A symmetric adjacency matrix.
#' @param threshold Smallest diffusion importance kept.
#' @return One value per node.
ref_a2_renewed_coreness <- function(graph, threshold = 2) {
  adjacency <- ref_a2_binary(graph)
  n <- nrow(adjacency)
  closed <- \(i) c(i, which(adjacency[i, ] != 0))
  # |N(j) \ N[i]|: the neighbours of j outside the closed neighbourhood of i
  leaving <- outer(seq_len(n), seq_len(n), Vectorize(\(i, j) length(setdiff(which(adjacency[j, ] != 0), closed(i)))))
  importance <- (leaving + t(leaving)) / 2
  residual <- adjacency * (importance >= threshold)
  dimnames(residual) <- dimnames(graph)
  ref_a2_kcore(residual)
}

#' Weighted k-shell (Garas, Schweitzer and Havlin 2012): weights divided by their mean, then by their smallest
#' value, rounded to whole numbers; nodes peeled at integer thresholds of the generalised degree
#' (k^alpha s^beta)^(1 / (alpha + beta)) on the remaining subgraph
#'
#' @param graph A symmetric weight matrix.
#' @param alpha,beta The exponents of degree and strength.
#' @return One value per node.
ref_a2_weighted_kshell <- function(graph, alpha = 1, beta = 1) {
  adjacency <- ref_a2_binary(graph)
  values <- graph[adjacency != 0]
  scaled <- values / mean(values)
  integer_weights <- adjacency
  integer_weights[adjacency != 0] <- floor(scaled / min(scaled) + 0.5)
  generalised <- \(alive) {
    kept <- outer(alive, alive)
    degree <- rowSums(adjacency * kept)
    strength <- rowSums(integer_weights * kept)
    ifelse(degree > 0, (degree^alpha * strength^beta)^(1 / (alpha + beta)), 0)
  }
  ref_a2_integer_shells(generalised, graph)
}

#' Extended neighbourhood coreness (Bae and Kim 2014): the sum over the neighbours j of i of the sum of the core
#' numbers of the neighbours of j
#'
#' @param graph A symmetric adjacency matrix.
#' @return One value per node.
ref_a2_extended_coreness <- function(graph) {
  adjacency <- ref_a2_binary(graph)
  core <- ref_a2_kcore(graph)
  ref_a2_per_node(graph, \(i) sum(vapply(which(adjacency[i, ] != 0),
                                         \(j) sum(core[which(adjacency[j, ] != 0)]), numeric(1))))
}

#' Centroid value (Wuchty and Stadler 2003; Koschuetzki et al. 2005): min over u != v of gamma(v, u) - gamma(u, v),
#' with gamma(v, u) the number of nodes strictly closer to v than to u. (Until 2026-11-22 this helper let u range
#' over v as well, where the difference is 0, copying cograph's old reading; the definition excludes u = v.)
#'
#' @param distances A distance matrix.
#' @return One value per node.
ref_a2_centroid <- function(distances) {
  n <- nrow(distances)
  closer <- \(v, u) sum(distances[v, ] < distances[u, ])
  ref_a2_per_node(distances, \(v) min(vapply(setdiff(seq_len(n), v), \(u) closer(v, u) - closer(u, v), numeric(1))))
}
