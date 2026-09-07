#' Benchmark-centrality sums over non-backtracking walks
#'
#' Equation (1) of Liu, Tang, Zhou and Do (arXiv:1511.00441v1, p. 4) is a
#' chain of *nested* sums: the second term runs over `j` in `Gamma_i`, the
#' third over `l` in `Gamma_j \ i`, the fourth over `m` in `Gamma_l \ j`, and
#' so on, each level excluding only the node the walk just came from. The
#' `k`-th term is therefore a sum of the benchmark centrality over the
#' endpoints of the **non-backtracking walks of length `k` starting at `i`**,
#' each walk contributing once. A walk may return to a node it visited
#' earlier, including `i` itself; only the immediate predecessor is barred.
#'
#' Writing `A_k` for the matrix of non-backtracking walk counts, this returns
#' `S_k = A_k theta` for `k = 1, ..., order`. The `A_k` obey
#'
#' ```
#' A_1 = A,  A_2 = A^2 - D,  A_k = A A_{k-1} - (D - I) A_{k-2}   (k >= 3),
#' ```
#'
#' which is exact on any simple graph. Proof: `A A_{k-1}` counts a
#' non-backtracking `(k-1)`-walk from `i` to `u` followed by an edge `u-j`,
#' so it is `A_k` plus the walks whose last step backtracks, that is those
#' whose `(k-1)`-walk ended `... -> j -> u`. The number of non-backtracking
#' `(k-1)`-walks from `i` to `u` with penultimate node `j` is
#' `A_{k-2}[i, j]` minus the ones that would have to step `u -> j -> u`, so
#' summing over the `d_j` neighbours `u` of `j` gives
#' `d_j A_{k-2}[i, j] - A_{k-2}[i, j]` for `k >= 3`, because every
#' non-backtracking walk of length `k - 2 >= 1` ending at `j` has exactly one
#' penultimate node. At `k = 2` the walk of length zero has no penultimate
#' node, so the correction is `d_j A_0[i, j] = D` instead of `D - I`, which
#' is why the second step is a separate base case. The recursion was also
#' checked against brute-force walk enumeration on every labeled simple
#' undirected graph through five vertices and on random graphs; see
#' `local_testing_and_equivalence/batch42/`.
#'
#' Because `D - I` is diagonal it commutes with the projection onto `theta`,
#' so only the vectors `S_k` are ever formed: `O(n^2)` work per step, no
#' `n * n` walk matrices.
#'
#' @param b Symmetric binary adjacency matrix with a zero diagonal.
#' @param theta Numeric benchmark centrality, one entry per node.
#' @param order Nonnegative integer number of steps.
#' @return A list of `order` numeric vectors, the `k`-step sums `S_k`. An
#'   `order` of zero gives an empty list.
#' @keywords internal
#' @noRd
.cg_nb_walk_sums <- function(b, theta, order) {
  order <- as.integer(order)
  if (order < 1L) return(list())
  degree <- rowSums(b)
  out <- vector("list", order)
  previous <- theta                       # S_0 = A_0 theta = theta
  current <- as.numeric(b %*% theta)      # S_1 = A theta
  out[[1L]] <- current
  # Each step needs the two before it, so the levels cannot be vectorised
  # across k; the work inside a step already is.
  for (k in seq_len(order - 1L) + 1L) {
    correction <- if (k == 2L) degree else degree - 1
    following <- as.numeric(b %*% current) - correction * previous
    out[[k]] <- following
    previous <- current
    current <- following
  }
  out
}
