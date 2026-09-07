#' Extended degree, E-shell position and HCC/EHCC (Liu and Zheng 2023)
#'
#' Liu and Zheng (2023), Scientific Reports 13:3197, combine a local and a
#' global characteristic. Journal page 3, equation (3), widens the degree
#' into the *extended degree*
#'
#' ```
#' k^ex(u) = delta * k(u) + (1 - delta) * sum_{v in phi(u)} k(v)
#' ```
#'
#' over the open 1-order neighbourhood `phi(u)`, with `delta` in `[0, 1]`
#' and `delta = 1` recovering the classical degree. The *E-shell hierarchy
#' decomposition* on the same page then peels the graph: in each iteration
#' it removes **every** node attaining the current minimum extended degree,
#' recomputes the extended degrees on what is left, and tags the removed
#' nodes with the iteration number as their position index `pos(u)`.
#' Equations (4) and (5), page 3, are
#'
#' ```
#' HCC(u)  = k^ex(u) / k^ex_max + pos(u) / pos_max
#' EHCC(u) = HCC(u) + sum_{v in phi(u)} HCC(v)
#' ```
#'
#' Three readings are fixed by the paper's own worked example (page 4,
#' Tables 1-3, on the 10-node 14-edge graph of its figure 1) rather than by
#' its prose, and each was checked against every printed value:
#'
#' * **Step 3 prints `arg max` and must be read as `arg min`.** The same
#'   step calls `S_p` "the set of minimum nodes", the preceding paragraph
#'   says "the nodes with minimum extended degree are found and deleted",
#'   and Table 2's column is headed "Minimum extended degree" with the
#'   increasing values 2, 2.5, 3, 4.5, 5, 6. Read literally, Step 3 would
#'   produce a different measure entirely.
#' * **The peel recomputes, the ratios do not.** Step 6 updates the
#'   extended degrees on the residual graph, which is what makes Table 2's
#'   minima 2, 2.5, 3, 4.5, 5, 6 rather than the original-graph values;
#'   but `k^ex(u)` in equation (4) is the **original**-graph extended
#'   degree, as the printed `HCC(a) = 4.5/11 + 4/6` shows, and node `d`
#'   settles it: its original 9.5 gives the printed 1.86 while its
#'   residual 6 at removal time would give 1.55.
#' * **`k^ex_max` and `pos_max` are global constants**, the largest
#'   original extended degree (11 in the example) and the number of
#'   peeling rounds (6). Neither is component-local, so adding a
#'   disconnected component can change every score.
#'
#' Ties need no ordering rule because a round removes the whole minimum
#' set at once; the example's rounds 4 and 6 remove `{a, b}` and
#' `{d, e, f, g}` together. Production compares the round's extended
#' degrees against the round minimum within a relative tolerance, because
#' `delta * k + (1 - delta) * S` is exact in binary floating point only for
#' a dyadic `delta`. At the source's `delta = 0.5`, and at 0, 1/4, 1/2, 3/4
#' and 1, every value is exact and the tolerance never fires.
#'
#' For `delta` in the source's `[0, 1]` the extended degree is nonnegative
#' and is zero exactly at an isolate, so an isolate is always removed in
#' the first round. `k^ex_max` is then zero exactly on an edgeless graph,
#' where equation (4)'s first term is `0/0` at every node; it is written as
#' **zero**, leaving the E-shell term, which is `1/1` because one round
#' removes everything. A singleton graph therefore scores 1.
#'
#' @param b Adjacency matrix; direction, weights, loops and parallel edges
#'   are dropped to the simple undirected skeleton the source defines on.
#' @param delta Weight on the node's own degree, a single number in
#'   `[0, 1]`; the source's value is 0.5.
#' @return A `data.frame` with one row per node in input order and the
#'   columns `degree` (`k(u)`), `kex` (`k^ex(u)`, equation 3), `pos`
#'   (the E-shell position index), `hcc` (equation 4) and `ehcc`
#'   (equation 5). The number of peeling rounds is carried on the result
#'   as the `pos_max` attribute and the largest extended degree as
#'   `kex_max`.
#' @keywords internal
#' @noRd
.cg_hcc_terms <- function(b, delta = 0.5) {
  if (!is.numeric(delta) || length(delta) != 1L || !is.finite(delta) ||
        delta < 0 || delta > 1) {
    message <- paste("`hcc_delta` must be a single number in [0, 1],",
                     "the source's stated domain")
    stop(errorCondition(message, class = "cograph_bad_parameter", call = NULL))
  }
  a <- .cg_undirected_view(b)
  diag(a) <- 0
  n <- nrow(a)
  if (is.null(n) || n == 0L) {
    empty <- data.frame(degree = numeric(), kex = numeric(),
                        pos = numeric(), hcc = numeric(), ehcc = numeric())
    attr(empty, "pos_max") <- 0
    attr(empty, "kex_max") <- 0
    return(empty)
  }
  degree <- rowSums(a)
  kex <- delta * degree + (1 - delta) * as.numeric(a %*% degree)
  pos <- numeric(n)
  alive <- rep(TRUE, n)
  residual <- degree
  rounds <- 0L
  # Each round's minimum depends on the degrees the previous round left
  # behind, so there is nothing to vectorise across rounds. At least one
  # node leaves per round, so the loop runs at most n times.
  while (any(alive)) {
    rounds <- rounds + 1L
    reachable <- as.numeric(a %*% (residual * alive))
    current <- delta * residual + (1 - delta) * reachable
    current[!alive] <- NA_real_
    smallest <- min(current, na.rm = TRUE)
    # A dyadic delta makes every value exact and this tolerance never
    # fires; a general delta can put a genuine tie a few ulps apart.
    slack <- 64 * .Machine$double.eps * max(1, abs(smallest))
    batch <- which(alive & current <= smallest + slack)
    pos[batch] <- rounds
    alive[batch] <- FALSE
    residual <- residual - rowSums(a[, batch, drop = FALSE])
  }
  kex_max <- max(kex)
  # An edgeless graph has every extended degree zero: equation (4)'s first
  # term is 0/0 at every node and is written as zero, which leaves the
  # E-shell term alone rather than returning NaN.
  share <- if (kex_max > 0) kex / kex_max else numeric(n)
  hcc <- share + pos / rounds
  out <- data.frame(degree = degree, kex = kex, pos = pos, hcc = hcc,
                    ehcc = hcc + as.numeric(a %*% hcc))
  attr(out, "pos_max") <- rounds
  attr(out, "kex_max") <- kex_max
  out
}
