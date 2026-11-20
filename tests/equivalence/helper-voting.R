# The VoteRank family and EnRenew written from their papers in base R, for test-centrality-voting.R. Nothing here
# calls cograph. Every procedure is an election: score the nodes not yet elected, elect the best, change the
# state that decides the next round, repeat until every node is placed. The node elected in round r scores
# (n - r + 1) / n, the form in which cograph reports the order. The papers leave ties open: a tie (scores within
# 1e-9 of the best, so that the order of a floating-point sum cannot decide) goes to the first node in input order.

#' Run an election to the end
#'
#' @param n Number of nodes.
#' @param state The starting state (whatever `score` and `update` read).
#' @param score Function of (state, elected) returning one score per node.
#' @param update Function of (state, winner, elected) returning the next state; `elected` excludes the winner.
#' @param names Node names.
#' @return The score (n - r + 1) / n of each node, named, with r its round of election.
vr_elect <- function(n, state, score, update, names) {
  stopifnot("`n` must be one whole number" = length(n) == 1L && n >= 1)
  round <- function(state, elected) {
    if (length(elected) == n) return(elected)
    scores <- score(state, elected)
    open <- !seq_len(n) %in% elected
    winner <- which(open & scores >= max(scores[open]) - 1e-9)[[1L]]
    round(update(state, winner, elected), c(elected, winner))
  }
  order <- round(state, integer(0))
  stats::setNames((n - match(seq_len(n), order) + 1) / n, names)
}

#' The simple undirected 0/1 neighbour matrix: direction, weights and loops dropped
vr_neighbours <- function(graph) {
  binary <- (graph != 0) * 1
  binary <- pmax(binary, t(binary))
  diag(binary) <- 0
  binary
}

#' Hop distances by breadth-first search on the 0/1 neighbour matrix
vr_hops <- function(binary) {
  n <- nrow(binary)
  start <- matrix(Inf, n, n)
  diag(start) <- 0
  Reduce(\(d, k) {
    frontier <- (d == k - 1L) * 1
    reached <- (frontier %*% binary) > 0 & is.infinite(d)
    d[reached] <- k
    d
  }, seq_len(max(n - 1L, 1L)), start)
}

#' k-shell index by peeling (Kitsak et al. 2010): remove every node of degree at most k, repeatedly, then raise k
vr_kshell <- function(binary) {
  n <- nrow(binary)
  peel <- function(shell, left, k) {
    if (!any(left)) return(shell)
    degree <- as.numeric(binary[, left, drop = FALSE] %*% rep(1, sum(left)))
    out <- left & degree <= k
    if (any(out)) return(peel(replace(shell, out, k), left & !out, k))
    peel(shell, left, k + 1)
  }
  peel(numeric(n), rep(TRUE, n), 0)
}

#' WVoteRank (Sun, Chen, He and Ch'ng 2019, eq. 2): s_v = sqrt(|N(v)| sum_{i in N(v)} va_i w_vi); the elected node's
#' ability becomes 0 and each neighbour's drops by 1 / <w>, <w> the average strength (2 sum w / n, as the paper's
#' Figure 1 requires), floored at 0
vr_wvoterank <- function(graph) {
  weights <- pmax(graph, t(graph))
  diag(weights) <- 0
  binary <- (weights != 0) * 1
  degree <- rowSums(binary)
  discount <- 1 / (sum(weights) / nrow(weights))
  vr_elect(nrow(graph), rep(1, nrow(graph)),
           score = \(ability, elected) sqrt(degree * as.numeric(weights %*% ability)),
           update = \(ability, winner, elected) {
             hit <- binary[winner, ] > 0
             ability[hit] <- pmax(0, ability[hit] - discount)
             replace(ability, winner, 0)
           }, names = rownames(graph))
}

#' NCVoteRank (Kumar and Panda 2020, as restated by the Centrality Zoo): s_u = sum_{v in N(u)} va_v (theta +
#' (1 - theta) nc_v), nc_v the sum of the neighbours' k-shell indices scaled by its maximum; the elected node's
#' ability becomes 0, its neighbours lose 1 / <k> and the nodes two steps away lose 1 / (2 <k>), floored at 0
vr_ncvoterank <- function(graph, theta = 0.5) {
  binary <- vr_neighbours(graph)
  mean_degree <- mean(rowSums(binary))
  coreness <- as.numeric(binary %*% vr_kshell(binary))
  weight <- theta + (1 - theta) * coreness / max(coreness)
  hops <- vr_hops(binary)
  vr_elect(nrow(graph), rep(1, nrow(graph)),
           score = \(ability, elected) as.numeric(binary %*% (ability * weight)),
           update = \(ability, winner, elected) {
             one <- hops[winner, ] == 1
             two <- hops[winner, ] == 2
             ability[one] <- pmax(0, ability[one] - 1 / mean_degree)
             ability[two] <- pmax(0, ability[two] - 1 / (2 * mean_degree))
             replace(ability, winner, 0)
           }, names = rownames(graph))
}

#' VoteRank++ (Liu, Li, Fang and Yao 2021, as in the authors' code): va_i = ln(1 + k_i / k_max); voter j gives
#' neighbour i the share k_i / sum of the degrees of j's neighbours not yet elected; s_i = sqrt(k_i sum_j va_j
#' share_ji); after an election the winner's ability is 0, its neighbours' abilities are multiplied by lambda and
#' those of the nodes two steps away not yet elected by sqrt(lambda)
vr_voterank_plus <- function(graph, lambda = 0.1) {
  binary <- vr_neighbours(graph)
  n <- nrow(binary)
  degree <- rowSums(binary)
  hops <- vr_hops(binary)
  vr_elect(n, log(1 + degree / max(degree)),
           score = \(ability, elected) {
             open <- !seq_len(n) %in% elected
             received <- vapply(seq_len(n), \(i) {
               voters <- which(binary[i, ] > 0)
               sum(vapply(voters, \(j) {
                 pool <- sum(degree[binary[j, ] > 0 & open])
                 if (pool > 0) ability[[j]] * degree[[i]] / pool else 0
               }, numeric(1)))
             }, numeric(1))
             ifelse(open, sqrt(degree * received), 0)
           },
           update = \(ability, winner, elected) {
             open <- !seq_len(n) %in% c(elected, winner)
             one <- hops[winner, ] == 1
             two <- hops[winner, ] == 2 & open
             ability[one] <- ability[one] * lambda
             ability[two] <- ability[two] * sqrt(lambda)
             replace(ability, winner, 0)
           }, names = rownames(graph))
}

#' EnRenew (Guo, Yang, Guo, Pan and Chen 2020, eq. 1 and Algorithm 1): E_v = sum_{u in N(v)} -p_uv ln p_uv with
#' p_uv = k_u / sum_{l in N(v)} k_l; after an election every term H_vu with u at distance d - 1 and v at distance d
#' from the winner (d = 1 .. depth) is multiplied by 1 - 1 / (2^(d - 1) ln <k>)
vr_enrenew <- function(graph, depth = 2L) {
  binary <- vr_neighbours(graph)
  n <- nrow(binary)
  degree <- rowSums(binary)
  share <- t(t(binary) * degree) / as.numeric(binary %*% degree)
  entropy <- ifelse(binary > 0, -share * log(share), 0)
  hops <- vr_hops(binary)
  factor <- 1 - 1 / (2^(seq_len(depth) - 1) * log(sum(degree) / n))
  vr_elect(n, entropy,
           score = \(terms, elected) rowSums(terms),
           update = \(terms, winner, elected) Reduce(\(terms, d) {
             outer_ring <- hops[winner, ] == d
             inner_ring <- hops[winner, ] == d - 1
             terms[outer_ring, inner_ring] <- terms[outer_ring, inner_ring] * factor[[d]]
             terms
           }, seq_len(depth), terms), names = rownames(graph))
}
