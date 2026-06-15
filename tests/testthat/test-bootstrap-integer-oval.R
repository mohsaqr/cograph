# Bootstrap plots: oval layout default for undirected co-occurrence nets,
# and ".00" suppression on integer-valued (count) weight matrices.

capture_splot_args <- function(expr) {
  .cograph_test_splot_call <<- NULL
  trace(
    "splot",
    where = asNamespace("cograph"),
    tracer = quote(.cograph_test_splot_call <<- as.list(match.call(expand.dots = TRUE))),
    print = FALSE
  )
  on.exit({
    untrace("splot", where = asNamespace("cograph"))
    rm(.cograph_test_splot_call, envir = .GlobalEnv)
  }, add = TRUE)
  with_temp_png(force(expr))
  .cograph_test_splot_call
}

make_net_bootstrap <- function(weights, directed = FALSE) {
  nodes <- rownames(weights)
  structure(
    list(
      original = list(
        weights = weights,
        directed = directed,
        nodes = data.frame(label = nodes)
      ),
      p_values = matrix(0.01, nrow(weights), ncol(weights), dimnames = dimnames(weights)),
      ci_level = 0.05
    ),
    class = "net_bootstrap"
  )
}

int_weights <- function() {
  nodes <- c("plan", "discuss", "consensus")
  w <- matrix(0, 3, 3, dimnames = list(nodes, nodes))
  w[upper.tri(w)] <- c(266, 708, 194)
  w + t(w)
}

float_weights <- function() {
  nodes <- c("a", "b", "c")
  w <- matrix(0, 3, 3, dimnames = list(nodes, nodes))
  w[upper.tri(w)] <- c(0.31, 0.52, 0.74)
  w + t(w)
}

test_that("undirected net_bootstrap defaults to oval layout", {
  call <- capture_splot_args(
    splot.net_bootstrap(make_net_bootstrap(int_weights()), display = "full")
  )
  expect_identical(call[["layout"]], "oval")
})

test_that("integer net_bootstrap strips .00 via weight_digits / edge_label_digits = 0", {
  call <- capture_splot_args(
    splot.net_bootstrap(make_net_bootstrap(int_weights()), display = "full")
  )
  expect_identical(call[["weight_digits"]], 0L)
  expect_identical(call[["edge_label_digits"]], 0L)
})

test_that("float net_bootstrap keeps decimal weights (no forced rounding)", {
  call <- capture_splot_args(
    splot.net_bootstrap(make_net_bootstrap(float_weights()), display = "full")
  )
  expect_null(call[["weight_digits"]])
  expect_null(call[["edge_label_digits"]])
})

test_that("user-supplied weight_digits / layout override integer defaults", {
  call <- capture_splot_args(
    splot.net_bootstrap(
      make_net_bootstrap(int_weights()),
      display = "full",
      weight_digits = 2,
      layout = "spring"
    )
  )
  expect_equal(call[["weight_digits"]], 2)
  expect_identical(call[["layout"]], "spring")
})

test_that("integer tna_bootstrap strips .00 as well", {
  nodes <- c("plan", "discuss", "consensus")
  w <- matrix(0, 3, 3, dimnames = list(nodes, nodes))
  w[upper.tri(w)] <- c(266, 708, 194)
  w <- w + t(w)
  boot <- structure(
    list(
      weights = w,
      weights_orig = w,
      p_values = matrix(0.01, 3, 3, dimnames = dimnames(w))
    ),
    class = c("tna_bootstrap", "list")
  )
  call <- capture_splot_args(splot.tna_bootstrap(boot, display = "full"))
  expect_identical(call[["weight_digits"]], 0L)
  expect_identical(call[["edge_label_digits"]], 0L)
})
