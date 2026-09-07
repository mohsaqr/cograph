#' Global and hybrid global structure models
#' @param b Simple undirected binary adjacency matrix.
#' @param core Original graph core numbers.
#' @param model One of gsm, hgsm or igsm.
#' @param normalized Whether to divide final scores by their maximum.
#' @return Numeric node scores; raw overflow errors, normalized scores
#'   are evaluated directly in logarithmic form.
#' @keywords internal
#' @noRd
.cg_global_structure <- function(b, core, model = "gsm", normalized = FALSE) {
  n <- nrow(b)
  if (n <= 1L || !any(b != 0)) return(numeric(n))
  log_self <- core / n
  exponent <- 1
  if (model == "hgsm") {
    log_self <- log_self * rowSums(b)
    top <- max(log_self)
    log_mean <- top + log(mean(exp(log_self - top)))
    exponent <- ceiling(log_mean / log(2))
    log_mass <- log_self
  } else if (model == "igsm") {
    degree <- rowSums(b)
    log_self <- degree / n
    log_mass <- log(degree)
    exponent <- ceiling(log2(mean(degree)))
  } else {
    log_mass <- log(core)
  }
  d <- .cg_distances(b, "all")
  log_score <- vapply(seq_len(n), function(i) {
    keep <- is.finite(d[i, ]) & d[i, ] > 0
    if (!any(keep)) return(-Inf)
    terms <- log_mass[keep] - exponent * log(d[i, keep])
    top <- max(terms)
    log_self[i] + top + log(sum(exp(terms - top)))
  }, numeric(1))
  if (normalized) return(exp(log_score - max(log_score)))
  if (max(log_score) > log(.Machine$double.xmax)) {
    stop("global structure score exceeds double range; use normalized = TRUE",
         call. = FALSE)
  }
  result <- exp(log_score)
  if (any(!is.finite(result))) {
    stop("global structure score exceeds double range; use normalized = TRUE",
         call. = FALSE)
  }
  result
}
