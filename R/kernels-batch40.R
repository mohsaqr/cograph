#' Shell and removal stage of the k-shell decomposition (Li and Huang 2021)
#'
#' Peels the graph exactly as Algorithm 1 of Li and Huang (2021) prescribes.
#' The level `k` starts at one and only ever grows. Within a level, one
#' *stage* removes every remaining node whose current degree is at most `k`,
#' all at once; the next stage repeats on what is left, until every survivor
#' has degree greater than `k`. `q(k)` is the number of stages the level
#' needed, and `max_stage` is the largest `q(k)` over the levels that removed
#' at least one node.
#'
#' Algorithm 1 prints "Find all nodes in G with degree k" but terminates the
#' stage loop with "until All remaining nodes in G have degree > k", and the
#' paper's Methods definition of k-shell says "remove nodes whose degree
#' k <= 1 ... Until there are no nodes in the network with degree k <= 1".
#' Read as strict equality the pseudocode cannot terminate on a path of three
#' nodes, where removing both degree-one ends leaves a degree-zero centre. The
#' at-most reading is the only one consistent with the printed termination
#' condition and the prose, and it reproduces the paper's Table 2.
#'
#' Because `k` starts at one, an isolate is removed in the one-shell and gets
#' shell one, not the zero that `centrality(measures = "coreness")` reports.
#' Isolates carry no edges, so this changes no other node's shell or stage.
#'
#' Algorithm 1 also counts one final stage that removes nothing. That stage
#' can never raise the maximum above one, which every nonempty graph already
#' reaches, so counting only the productive stages gives the same `max_stage`.
#'
#' @param b Symmetric binary adjacency matrix with a zero diagonal.
#' @return List with integer vectors `shell` and `stage`, one entry per node,
#'   and the integer `max_stage`. An empty graph gives empty vectors and
#'   `max_stage` one.
#' @keywords internal
#' @noRd
.cg_shell_stage <- function(b) {
  n <- nrow(b)
  if (!n) return(list(shell = integer(0), stage = integer(0), max_stage = 1L))
  shell <- integer(n)
  stage <- integer(n)
  alive <- rep(TRUE, n)
  residual <- rowSums(b)
  k <- 1L
  max_stage <- 1L
  # Iterative peeling: each stage depends on the degrees the previous stage
  # left behind, so there is nothing to vectorise across stages.
  while (any(alive)) {
    q <- 0L
    repeat {
      batch <- which(alive & residual <= k)
      if (!length(batch)) break
      q <- q + 1L
      shell[batch] <- k
      stage[batch] <- q
      alive[batch] <- FALSE
      residual <- residual - rowSums(b[, batch, drop = FALSE])
    }
    if (q > max_stage) max_stage <- q
    k <- k + 1L
  }
  list(shell = shell, stage = stage, max_stage = max_stage)
}

#' Degree k-shell (DK) index of Li and Huang (2021), equations 1-2
#'
#' `k_s*(i) = k_s(i) + p(i) / (max_k q(k) + 1)` refines the shell with the
#' stage at which the node left it, and `DK(i) = k(i) + k_s*(i)` adds the
#' original degree. Degrees, shells and stages all belong to the supplied
#' graph; the denominator is one global maximum, so a node's DK depends on
#' the whole graph, disconnected components included.
#'
#' @param b Symmetric binary adjacency matrix with a zero diagonal.
#' @return Numeric vector of DK values, one per node.
#' @keywords internal
#' @noRd
.cg_dk_index <- function(b) {
  peel <- .cg_shell_stage(b)
  rowSums(b) + peel$shell + peel$stage / (peel$max_stage + 1)
}
