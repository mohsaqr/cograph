#' Weighted formal concept analysis centrality (Sun et al. 2017)
#'
#' Sun, Wang, Sheng, Hu, Wang and Shao (2017), *IEEE Access* 5:3777-3789,
#' Algorithm 1 and formula (3), page 3782. The adjacency matrix is read as
#' a Boolean context with the nodes as objects (rows) and as attributes
#' (columns). A formal concept is a pair `(T, P)` of node sets in which `P`
#' holds every attribute shared by all objects of `T` and `T` holds every
#' object that has all attributes of `P`. The weight of a node is
#'
#' ```
#' W_i = sum over the concepts (T, P) with i in P of |T| / |P|.       (3)
#' ```
#'
#' The intents `P` are exactly the intersections of sets of rows, together
#' with the full attribute set. They are built by intersecting the rows
#' one at a time with every intent found so far. Their number can grow
#' exponentially, so the construction stops with a classed condition once
#' more than `limit` intents exist.
#'
#' @param a Binary adjacency matrix with a zero diagonal, `a[i, j]` for the
#'   arc `i -> j`.
#' @param limit Largest number of intents.
#' @return Numeric vector; raises `cograph_enumeration_limit` when the
#'   number of concepts passes `limit`.
#' @keywords internal
#' @noRd
.cg_wfca <- function(a, limit = 1e6) {
  .cg_check_enumeration_limit(limit)
  n <- nrow(a)
  if (is.null(n) || n == 0L) return(numeric(0))
  has <- a != 0
  add_row <- function(intents, object) {
    met <- t(t(intents) & has[object, ])
    out <- unique(rbind(intents, met))
    if (nrow(out) > limit) .cg_stop_enumeration()
    out
  }
  intents <- Reduce(add_row, seq_len(n), matrix(TRUE, 1L, n))
  # Extent of an intent: the objects whose row contains it.
  extent <- vapply(seq_len(nrow(intents)), \(k) {
    sum(colSums(t(has) & intents[k, ]) == sum(intents[k, ]))
  }, numeric(1))
  size <- rowSums(intents)
  share <- ifelse(size > 0, extent / pmax(size, 1), 0)
  as.numeric(share %*% intents)
}
