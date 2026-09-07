#' Per-node triangle counts on a simple undirected skeleton
#'
#' `NTS(u)` in the notation of Wang, Yang, Liu and Ma (2021): the number of
#' triangles that contain `u`, equivalently the number of edges among the
#' neighbours of `u`. Computed by masking the common-neighbour matrix
#' `A %*% A` with `A` itself and halving, because
#' `sum_v A[u, v] (A %*% A)[u, v]` counts every triangle at `u` once from
#' each of its two other corners.
#'
#' The arithmetic is exact: `A` is a 0/1 matrix, so every entry of
#' `A %*% A` is a small integer held exactly in a double and the row sums
#' are exact integers.
#'
#' @param a Binary symmetric adjacency matrix with a zero diagonal, as
#'   returned by `.cg_undirected_view()`.
#' @return Numeric vector of triangle counts, one per node, in input order.
#' @keywords internal
#' @noRd
.cg_triangle_counts <- function(a) {
  n <- nrow(a)
  if (is.null(n) || n == 0L) return(numeric(0))
  rowSums(a * (a %*% a)) / 2
}

#' The Lhc index and its parts (Wang, Yang, Liu and Ma 2021)
#'
#' Wang, Yang, Liu and Ma (2021), *PLoS ONE* 16(5):e0251208, combine a
#' node's neighbour information with its topological location. Journal
#' page 3, equation (1), scores the *influence* of a node by a
#' distance-discounted sum over a ball around it, and page 4, equation (2),
#' turns that into the index by summing the influence over the node's own
#' neighbours:
#'
#' ```
#' C(v)   = sum_{u in Phi(v)}  k_u (1 + TP(u)) / d(uv)^2        (1)
#' Lhc(v) = sum_{w in tau(v)}  C(w)                             (2)
#' ```
#'
#' with `k_u` the degree of `u`, `d(uv)` the shortest-path distance,
#' `Phi(v)` the ball of radius `d` around `v` and `tau(v)` its "nearest
#' neighbourhood". The triangle share `TP(u)` is defined on the same page
#' as
#'
#' ```
#' TP(u) = NTS(u) / TNTS,   TNTS = sum_u NTS(u)
#' ```
#'
#' where `NTS(u)` is the number of triangles containing `u`. Page 4 states
#' "the distance ranged d is set to be 2", and page 7 reports a sweep over
#' `d` on eleven real networks finding "the optimal value of d is about
#' 2-3".
#'
#' **The normaliser is `TNTS`, not the number of triangles.** The paper
#' settles this itself, in the sentence after the definition: "the total
#' number of triangle structure exists in the network are `1/3 * TNTS`".
#' So `TNTS = 3 * Delta` with `Delta` the number of distinct triangles, and
#' `TP` sums to exactly one over the nodes. The Centrality Zoo's entry
#' 2.221 transcribes the structure of equations (1) and (2) correctly but
#' writes the denominator as "`Delta`, the total number of triangular
#' structures in the network", which read literally is three times too
#' small; on the Krackhardt kite that reading gives 125.45 where the paper
#' gives 100.15. The paper is followed.
#'
#' **`Phi(v)` is open and so is `tau(v)`.** `Phi(v)` is `1 <= d(u,v) <= d`:
#' `v` itself cannot be in it, since `d^2(vv) = 0` would divide by zero,
#' and the paper describes it as "the nearby nodes include but not bounded
#' the nearest neighbors". `tau(v)` is the open neighbourhood `N(v)`, "the
#' nearest neighbourhood of node v". A consequence the paper does not
#' remark on but which follows from its equations: `v` **does** contribute
#' to its own `Lhc(v)`, because `v` lies in `Phi(w)` at distance 1 for
#' every `w` in `tau(v)`.
#'
#' **Triangle-free graphs are a cograph decision, and an explicit one.**
#' `TNTS` is zero on every tree, star, path, even cycle and bipartite
#' graph, and `TP(u)` is then `0/0` at every node. The paper never
#' mentions the case. Because `TNTS = sum_u NTS(u)` with every term
#' nonnegative, `TNTS = 0` holds exactly when *every* numerator `NTS(u)` is
#' zero as well, so there is no share to distribute and no node has a claim
#' on one. `TP` is therefore written as **zero** on a triangle-free graph,
#' which leaves the index as the pure
#' `sum_{w in tau(v)} sum_{u in Phi(w)} k_u / d(uw)^2` -- the neighbour and
#' location half of the hybrid, with the triangle half contributing
#' nothing. The branch is taken by testing `TNTS` before any division, so
#' no `0/0` is ever evaluated; the alternatives, `NA` or an error, would
#' refuse every tree, which the source's own construction handles perfectly
#' well.
#'
#' **Raw scores are not component-local.** `TNTS` is a global sum, so
#' adding a disconnected component that carries a triangle rescales every
#' `TP` and therefore every score. Adding a component with *no* triangle --
#' an isolate included -- changes nothing, because it changes no degree, no
#' triangle and no finite distance inside the existing components. The
#' radius cutoff already excludes unreachable nodes, so no infinity arises.
#'
#' **Isolates score a derived zero.** An isolate has an empty `tau(v)`, so
#' equation (2) is an empty sum. A singleton graph and every node of an
#' edgeless graph score zero for the same reason, and an empty graph
#' returns no scores at all.
#'
#' @param b Adjacency matrix. Direction, weights, loops and parallel edges
#'   are dropped to the simple undirected skeleton the source defines on:
#'   `k_u` is a count, `d(uv)` a hop count and `NTS(u)` a combinatorial
#'   quantity, and the paper's networks are simple and undirected.
#' @param radius The `d` of equation (1), a single whole number of at least
#'   one. The source sets it to 2.
#' @return A `data.frame` with one row per node in input order and the
#'   columns `degree` (`k_u`), `nts` (`NTS(u)`), `tp` (`TP(u)`),
#'   `influence` (`C(v)`, equation 1) and `lhc` (equation 2). `TNTS` is
#'   carried on the result as the `tnts` attribute and the number of
#'   distinct triangles, `TNTS / 3`, as `triangles`.
#' @keywords internal
#' @noRd
.cg_lhc_terms <- function(b, radius = 2) {
  # `trunc()` returns an exactly representable value, so the integrality
  # test below is exact rather than a tolerance question.
  if (!is.numeric(radius) || length(radius) != 1L || !is.finite(radius) ||
        radius < 1 || radius != trunc(radius)) {
    message <- paste("`lhc_radius` must be a single whole number of at",
                     "least one; it is the `d` of equation (1), which the",
                     "source sets to 2")
    stop(errorCondition(message, class = "cograph_bad_parameter", call = NULL))
  }
  a <- .cg_undirected_view(b)
  n <- nrow(a)
  if (is.null(n) || n == 0L) {
    empty <- data.frame(degree = numeric(), nts = numeric(), tp = numeric(),
                        influence = numeric(), lhc = numeric())
    attr(empty, "tnts") <- 0
    attr(empty, "triangles") <- 0
    return(empty)
  }

  degree <- rowSums(a)
  nts <- .cg_triangle_counts(a)
  tnts <- sum(nts)
  # TNTS is a sum of nonnegative integers, so TNTS == 0 holds exactly when
  # every NTS(u) is zero: the graph is triangle-free and no node has a
  # share of anything. The 0/0 of TP is never evaluated.
  tp <- if (tnts > 0) nts / tnts else rep(0, n)

  d <- .cg_distances(a, "all")
  # Phi(v) is the OPEN ball: the focal node is excluded because d^2(vv)
  # would be zero, and unreachable nodes fall outside the radius anyway.
  inside <- is.finite(d) & d >= 1 & d <= radius
  discount <- matrix(0, n, n)
  discount[inside] <- 1 / d[inside]^2
  # Equation (1): C(v) = sum_u discount[v, u] k_u (1 + TP(u)); the distance
  # matrix is symmetric on the undirected skeleton, so the row and column
  # readings coincide.
  influence <- as.numeric(discount %*% (degree * (1 + tp)))
  # Equation (2): Lhc(v) = sum over the open neighbourhood of v.
  lhc <- as.numeric(a %*% influence)

  out <- data.frame(degree = degree, nts = nts, tp = tp,
                    influence = influence, lhc = lhc)
  rownames(out) <- NULL
  attr(out, "tnts") <- tnts
  attr(out, "triangles") <- tnts / 3
  out
}
