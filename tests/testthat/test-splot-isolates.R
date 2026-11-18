# splot(isolates = FALSE) and the group plot

skip_on_cran()

padded_matrix <- function() {
  # a, b, c tied; d has only a self-loop; e and f have nothing
  w <- matrix(0, 6, 6, dimnames = list(letters[1:6], letters[1:6]))
  w["a", "b"] <- 0.5
  w["b", "c"] <- 0.3
  w["c", "a"] <- 0.2
  w["d", "d"] <- 0.4
  w
}

people_groups <- function() {
  skip_if_not_installed("Nestimate")
  log <- data.frame(
    course = c(rep("c1", 5), rep("c2", 4)),
    thread = c(1, 1, 1, 1, 2, 3, 3, 3, 3),
    user = c("s1", "s2", "s1", "s3", "s9", "s4", "s5", "s4", "s6"),
    time = as.POSIXct("2026-01-01", tz = "UTC") + seq_len(9) * 60
  )
  Nestimate::build_network(log, method = "reverse", actor = "thread",
                           action = "user", time = "time", group = "course")
}

with_null_device <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(code)
}

test_that("isolates = FALSE draws only nodes with a tie; a self-loop is a tie", {
  w <- padded_matrix()
  with_null_device({
    all_nodes <- splot(w)
    shown <- splot(w, isolates = FALSE)
  })
  expect_equal(n_nodes(all_nodes), 6)
  expect_identical(get_labels(shown), c("a", "b", "c", "d"))
  expect_equal(to_matrix(shown), w[1:4, 1:4], ignore_attr = TRUE)
})

test_that("the default draws every node", {
  w <- padded_matrix()
  with_null_device(res <- splot(w, isolates = TRUE))
  expect_equal(n_nodes(res), 6)
  expect_true(formals(splot)$isolates)
})

test_that("the result does not depend on where the isolates sit", {
  w <- padded_matrix()
  order <- c(6, 1, 5, 2, 3, 4)
  with_null_device({
    shown <- splot(w, isolates = FALSE)
    shuffled <- splot(w[order, order], isolates = FALSE)
  })
  expect_setequal(get_labels(shuffled), get_labels(shown))
  expect_equal(to_matrix(shuffled)[get_labels(shown), get_labels(shown)],
               to_matrix(shown), ignore_attr = TRUE)
})

test_that("per-node arguments follow the nodes that stay", {
  keep <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  args <- list(labels = LETTERS[1:6], node_fill = rep(c("red", "blue"), 3),
               node_size = 5, layout = cbind(1:6, 6:1),
               edge_color = c("a", "b", "c", "d", "e", "f"), title = "t")
  kept <- cograph:::.keep_node_arguments(args, keep)
  expect_identical(kept$labels, LETTERS[1:4])
  expect_identical(kept$node_fill, c("red", "blue", "red", "blue"))
  expect_identical(kept$node_size, 5)
  expect_equal(kept$layout, cbind(1:4, 6:3))
  expect_length(kept$edge_color, 6)     # an edge argument is not per node
  expect_identical(kept$title, "t")

  w <- padded_matrix()
  with_null_device(expect_no_error(
    splot(w, isolates = FALSE, labels = LETTERS[1:6],
          node_fill = rep(c("#E69F00", "#56B4E9"), 3),
          layout = cbind(cos(1:6), sin(1:6)))
  ))
})

test_that("every input type gives the same shown nodes", {
  skip_if_not_installed("igraph")
  w <- padded_matrix()
  inputs <- list(
    matrix = w,
    igraph = igraph::graph_from_adjacency_matrix(w, mode = "directed",
                                                 weighted = TRUE),
    cograph = as_cograph(w)
  )
  with_null_device(shown <- lapply(inputs, function(x) {
    get_labels(splot(x, isolates = FALSE))
  }))
  lapply(shown, function(labels) expect_identical(labels, c("a", "b", "c", "d")))
})

test_that("a Nestimate network keeps its class and fields", {
  grp <- people_groups()
  member <- grp$c1
  kept <- cograph:::.keep_nodes_in_format(member, cograph:::.tied_nodes(member))
  expect_s3_class(kept, "netobject")
  expect_identical(kept$method, member$method)
  expect_equal(kept$n_nodes, 3)
  expect_equal(dim(kept$weights), c(3L, 3L))
  expect_identical(get_labels(kept), c("s1", "s2", "s3"))
})

test_that("a network without ties is drawn whole, with a warning", {
  empty <- matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
  with_null_device(
    expect_warning(res <- splot(empty, isolates = FALSE),
                   class = "cograph_empty_network")
  )
  expect_equal(n_nodes(res), 3)
})

test_that("isolates is validated", {
  with_null_device({
    expect_error(splot(padded_matrix(), isolates = NA),
                 class = "cograph_bad_parameter")
    expect_error(splot(padded_matrix(), isolates = "yes"),
                 class = "cograph_bad_parameter")
  })
})

test_that("group plots take isolates and select one group with i", {
  grp <- people_groups()
  with_null_device({
    expect_no_error(splot(grp))
    expect_no_error(splot(grp, isolates = FALSE))
    expect_no_error(plot(grp, isolates = FALSE))
    expect_no_error(splot(grp, i = "c2", isolates = FALSE))
    expect_no_error(splot(grp, i = 1))
    expect_error(splot(grp, i = "nope"), class = "cograph_bad_selection")
    expect_error(splot(grp, i = 3), class = "cograph_bad_selection")
  })
})

test_that("the group plot restores the caller's panel layout", {
  grp <- people_groups()
  with_null_device({
    before <- graphics::par("mfrow")
    splot(grp, isolates = FALSE)
    expect_identical(graphics::par("mfrow"), before)
  })
})

test_that("tna groups accept isolates", {
  skip_if_not_installed("tna")
  model <- tna::group_model(tna::group_regulation,
                            group = rep(c("A", "B"), 1000))
  with_null_device({
    expect_no_error(splot(model, isolates = FALSE))
    expect_no_error(splot(model$A, isolates = FALSE))
  })
})
