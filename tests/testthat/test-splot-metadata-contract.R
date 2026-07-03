test_that("meta$splot network defaults are applied and user args override them", {
  mat <- matrix(c(0, 1, 0,
                  0, 0, 1,
                  1, 0, 0), 3, 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  net <- as_cograph(mat, directed = TRUE)
  net$meta$splot <- list(
    renderer = "network",
    defaults = list(
      layout = "circle",
      node_fill = "tomato",
      edge_labels = TRUE
    )
  )

  result <- with_temp_png(splot(net))
  expect_s3_class(result, "cograph_network")
  expect_true(all(result$nodes$node_fill == "tomato"))

  override <- with_temp_png(splot(net, node_fill = "steelblue"))
  expect_true(all(override$nodes$node_fill == "steelblue"))
})

test_that("meta$splot weight selects an alternate edge quantity", {
  mat <- matrix(c(0, 1, 2,
                  0, 0, 3,
                  4, 0, 0), 3, 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  adj_res <- matrix(c(0, 1.25, -2.5,
                      0, 0, 3.75,
                      -4.25, 0, 0), 3, 3, byrow = TRUE,
                    dimnames = dimnames(mat))

  net <- as_cograph(mat, directed = TRUE)
  net$adj_res <- adj_res
  net$edges$adj_res <- adj_res[cbind(net$edges$from, net$edges$to)]
  net$meta$splot <- list(
    renderer = "network",
    weight = "adj_res",
    defaults = list(edge_labels = TRUE, weight_digits = 2)
  )

  result <- with_temp_png(splot(net))
  expect_equal(result$weights, adj_res)
  expect_equal(
    result$edges$weight,
    adj_res[cbind(result$edges$from, result$edges$to)]
  )
})

test_that("meta$splot can route a non-classed object to a whitelisted renderer", {
  nodes <- c("A", "B", "C")
  weights <- matrix(c(0, 1, 2,
                      1, 0, 3,
                      2, 3, 0), 3, 3, byrow = TRUE,
                    dimnames = list(nodes, nodes))

  boot <- list(
    original = list(
      weights = weights,
      directed = FALSE,
      nodes = data.frame(label = nodes, stringsAsFactors = FALSE)
    ),
    p_values = matrix(0.01, 3, 3, dimnames = dimnames(weights)),
    ci_level = 0.05,
    meta = list(
      splot = list(
        renderer = "bootstrap",
        defaults = list(display = "full")
      )
    )
  )
  class(boot) <- "producer_bootstrap"

  result <- with_temp_png(splot(boot))
  expect_s3_class(result, "cograph_network")
})

test_that("meta$splot rejects unknown renderers and missing weights", {
  mat <- matrix(c(0, 1, 0, 0), 2, 2)
  net <- as_cograph(mat, directed = TRUE)

  net$meta$splot <- list(renderer = "not_a_renderer")
  expect_error(splot(net), "Unknown x\\$meta\\$splot\\$renderer")

  net$meta$splot <- list(renderer = "network", weight = "missing_column")
  expect_error(splot(net), "no matching edge column or matrix")
})

test_that("meta$splot weight matrix redefines the drawn edge set", {
  lab <- c("A", "B", "C")
  m <- matrix(c(0, 2, 0,
                0, 0, 3,
                0, 0, 0), 3, 3, byrow = TRUE, dimnames = list(lab, lab))
  net <- as_cograph(m, directed = TRUE)
  # A->C is nonzero ONLY in the alternate matrix (e.g. a residual at a
  # zero-count transition) — it must still be drawn
  adj <- matrix(c(0, 1.5, -2.2,
                  0, 0, 0.5,
                  0, 0, 0), 3, 3, byrow = TRUE, dimnames = list(lab, lab))
  net$adj_res <- adj
  net$meta$splot <- list(renderer = "network", weight = "adj_res")

  result <- with_temp_png(splot(net))
  expect_equal(result$weights, adj)
  expect_equal(nrow(result$edges), 3L)
  expect_true(any(abs(result$edges$weight + 2.2) < 1e-9))
})

test_that("meta$splot weight matrix aligns via dimnames for character edges", {
  lab <- c("A", "B", "C")
  m <- matrix(c(0, 2, 0,
                0, 0, 3,
                0, 0, 0), 3, 3, byrow = TRUE, dimnames = list(lab, lab))
  adj <- matrix(c(0, 1.5, 0,
                  0, 0, 0.5,
                  0, 0, 0), 3, 3, byrow = TRUE, dimnames = list(lab, lab))
  net <- structure(
    list(
      nodes = data.frame(id = 1:3, label = lab, name = lab,
                         stringsAsFactors = FALSE),
      edges = data.frame(from = c("A", "B"), to = c("B", "C"),
                         weight = c(2, 3), stringsAsFactors = FALSE),
      weights = m, directed = TRUE, adj_res = adj,
      meta = list(splot = list(renderer = "network", weight = "adj_res"))
    ),
    class = "cograph_network"
  )
  result <- with_temp_png(splot(net))
  expect_equal(sort(result$edges$weight), c(0.5, 1.5))
})

test_that("meta$splot edge-column weight keeps $weights consistent (never deletes it)", {
  lab <- c("A", "B", "C")
  m <- matrix(c(0, 2, 0,
                0, 0, 3,
                0, 0, 0), 3, 3, byrow = TRUE, dimnames = list(lab, lab))
  nob <- structure(
    list(
      nodes = data.frame(id = 1:3, label = lab, name = lab,
                         stringsAsFactors = FALSE),
      edges = data.frame(from = 1:2, to = 2:3, weight = c(2, 3),
                         count = c(9, 4)),
      weights = m, directed = TRUE, method = "relative",
      meta = list(splot = list(renderer = "netobject", weight = "count"))
    ),
    class = c("netobject", "cograph_network")
  )
  # splot.netobject renders x$weights — deleting it used to crash here
  result <- with_temp_png(splot(nob))
  expect_s3_class(result, "cograph_network")
  expect_equal(sort(result$edges$weight), c(4, 9))
})

test_that("user deprecated alias beats a meta$splot default for the new name", {
  m <- matrix(c(0, 1, 0,
                0, 0, 1,
                0, 0, 0), 3, 3, byrow = TRUE,
              dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  net <- as_cograph(m, directed = TRUE)
  net$meta$splot <- list(
    renderer = "network",
    defaults = list(edge_positive_color = "#009900")
  )

  rec <- new.env()
  orig <- get("handle_deprecated_param", envir = asNamespace("cograph"))
  on.exit(assignInNamespace("handle_deprecated_param", orig, ns = "cograph"),
          add = TRUE)
  assignInNamespace("handle_deprecated_param",
    function(new_val, old_val, new_name, old_name, new_val_was_set = NULL) {
      out <- orig(new_val, old_val, new_name, old_name, new_val_was_set)
      if (new_name == "edge_positive_color") rec$resolved <- out
      out
    }, ns = "cograph")

  suppressWarnings(with_temp_png(splot(net, positive_color = "blue")))
  expect_identical(rec$resolved, "blue")

  rec$resolved <- NULL
  with_temp_png(splot(net))
  expect_identical(rec$resolved, "#009900")
})

test_that("meta$splot validation rejects malformed specs exactly", {
  m <- matrix(c(0, 1, 0, 0), 2, 2)
  net <- as_cograph(m, directed = TRUE)

  net$meta$splot <- "difference"
  expect_error(splot(net), "must be a named list")

  net$meta$splot <- list(renderer = c("network", "difference"))
  expect_error(splot(net), "non-empty character scalar")

  net$meta$splot <- list(renderer = "network", weight = 1L)
  expect_error(splot(net), "must be a non-empty character scalar")

  # $ partial matching must not leak unrelated fields into the contract:
  # weight_digits at spec level is NOT the weight field
  net$meta$splot <- list(renderer = "network", weight_digits = 1)
  expect_silent(with_temp_png(splot(net)))
})
