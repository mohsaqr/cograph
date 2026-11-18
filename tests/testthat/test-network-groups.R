# centrality() and network_summary() on group network objects

skip_on_cran()

# Three courses with disjoint people: a message log, one row per message.
group_reply_net <- function() {
  skip_if_not_installed("Nestimate")
  saved_rng <- cograph:::.save_rng()
  on.exit(cograph:::.restore_rng(saved_rng), add = TRUE)
  set.seed(2)
  course_log <- function(course, people, n) {
    data.frame(course = course, thread = sample(seq_len(4), n, TRUE),
               user = sample(people, n, TRUE),
               time = as.POSIXct("2026-01-01", tz = "UTC") +
                 sort(sample(seq_len(10000), n)))
  }
  log <- rbind(course_log("c1", paste0("s", 1:6), 60),
               course_log("c2", paste0("s", 7:11), 50),
               course_log("c3", paste0("s", 12:20), 70))
  Nestimate::build_network(log, method = "reverse", actor = "thread",
                           action = "user", time = "time", group = "course")
}

test_that("network_summary gives one row per group on the group's own nodes", {
  grp <- group_reply_net()
  # the builder pads every group to the 20 people of the whole log
  expect_identical(unique(vapply(grp, function(n) nrow(n$nodes), integer(1))),
                   20L)

  res <- network_summary(grp)
  expect_s3_class(res, "data.frame")
  expect_identical(res$group, c("c1", "c2", "c3"))
  expect_identical(names(res)[1], "group")
  expect_equal(res$node_count, c(6, 5, 9))

  # each row equals the summary of that group's network without the padding
  alone <- network_summary(remove_isolates(grp$c2))
  expect_equal(subset(res, group == "c2", select = -group), alone,
               ignore_attr = TRUE)
})

test_that("a member without ties is counted in its own group only", {
  skip_if_not_installed("Nestimate")
  # s9 posts once, alone in a thread of course c1: present, no tie
  log <- data.frame(
    course = c(rep("c1", 5), rep("c2", 4)),
    thread = c(1, 1, 1, 1, 2, 3, 3, 3, 3),
    user = c("s1", "s2", "s1", "s3", "s9", "s4", "s5", "s4", "s6"),
    time = as.POSIXct("2026-01-01", tz = "UTC") + seq_len(9) * 60
  )
  grp <- Nestimate::build_network(log, method = "reverse", actor = "thread",
                                  action = "user", time = "time",
                                  group = "course")
  expect_equal(sum(grp$c1$weights["s9", ], grp$c1$weights[, "s9"]), 0)

  res <- network_summary(grp)
  expect_equal(res$node_count, c(4, 3))        # hand count: s1 s2 s3 s9 | s4 s5 s6
  expect_equal(res$component_count, c(2, 1))   # s9 is an isolate of c1
  cent <- centrality(grp, measures = "degree")
  expect_setequal(subset(cent, group == "c1")$node, c("s1", "s2", "s3", "s9"))
  expect_setequal(subset(cent, group == "c2")$node, c("s4", "s5", "s6"))
  expect_equal(subset(cent, node == "s9")$degree_all, 0)
})

test_that("shared_nodes = TRUE keeps the common node set", {
  grp <- group_reply_net()
  res <- network_summary(grp, shared_nodes = TRUE)
  expect_equal(res$node_count, c(20, 20, 20))
  expect_equal(subset(res, group == "c1", select = -group),
               network_summary(grp$c1), ignore_attr = TRUE)
})

test_that("centrality gives one row per node per group", {
  grp <- group_reply_net()
  res <- centrality(grp)
  expect_s3_class(res, "data.frame")
  expect_identical(names(res)[1:2], c("group", "node"))
  expect_equal(as.vector(table(res$group)), c(6, 5, 9))
  expect_setequal(subset(res, group == "c2")$node, paste0("s", 7:11))
  expect_equal(subset(res, group == "c3", select = -group),
               centrality(remove_isolates(grp$c3)), ignore_attr = TRUE)

  shared <- centrality(grp, shared_nodes = TRUE)
  expect_equal(as.vector(table(shared$group)), c(20, 20, 20))
})

test_that("arguments reach every group as the caller gave them", {
  grp <- group_reply_net()
  chosen <- c("degree", "betweenness")
  res <- centrality(grp, measures = chosen, mode = "in", digits = 2)
  alone <- centrality(remove_isolates(grp$c1), measures = chosen, mode = "in",
                      digits = 2)
  expect_equal(subset(res, group == "c1", select = -group), alone,
               ignore_attr = TRUE)

  detailed <- network_summary(grp, detailed = TRUE, digits = 5)
  expect_true("mean_degree" %in% names(detailed))
  expect_equal(nrow(detailed), 3)
})

test_that("group order does not change a group's values", {
  grp <- group_reply_net()
  flipped <- structure(rev(unclass(grp)), class = class(grp))
  res <- network_summary(grp)
  rev_res <- network_summary(flipped)
  expect_equal(subset(rev_res, group == "c1", select = -group),
               subset(res, group == "c1", select = -group),
               ignore_attr = TRUE)
})

test_that("tna group models keep tna conventions per group", {
  skip_if_not_installed("tna")
  model <- tna::group_model(tna::group_regulation,
                            group = rep(c("A", "B"), 1000))
  res <- centrality(model)
  expect_identical(unique(res$group), c("A", "B"))
  expect_equal(nrow(res), 18)
  expect_equal(subset(res, group == "A", select = -group),
               centrality(model$A), ignore_attr = TRUE)

  smry <- network_summary(model)
  expect_identical(smry$group, c("A", "B"))
  expect_equal(smry$node_count, c(9, 9))
})

test_that("a state absent from one tna group is dropped there only", {
  skip_if_not_installed("tna")
  # group A never visits "adapt"; the shared alphabet still gives it the state
  sequences <- tna::group_regulation
  in_a <- rep(c(TRUE, FALSE), 1000)
  sequences[in_a, ] <- lapply(sequences[in_a, ], function(state) {
    replace(state, which(state == "adapt"), "plan")
  })
  model <- tna::group_model(sequences, group = rep(c("A", "B"), 1000))
  expect_equal(nrow(model$A$weights), 9)

  res <- centrality(model)
  expect_false("adapt" %in% subset(res, group == "A")$node)
  expect_true("adapt" %in% subset(res, group == "B")$node)
  expect_equal(network_summary(model)$node_count, c(8, 9))
  expect_equal(nrow(centrality(model, shared_nodes = TRUE)), 18)
})

test_that("a group without ties or data warns and keeps the shared nodes", {
  grp <- group_reply_net()
  # filter every edge out; the verb warns that nothing is left
  expect_warning(
    expect_warning(grp$c1 <- threshold_edges(grp$c1, minimum = 1e9),
                   class = "cograph_isolates_created"),
    "removed all edges"
  )
  expect_equal(n_edges(grp$c1), 0)
  grp$c1$data <- NULL
  expect_warning(res <- network_summary(grp), class = "cograph_empty_group")
  expect_equal(res$node_count, c(20, 5, 9))
})

test_that("shared_nodes is validated", {
  grp <- group_reply_net()
  expect_error(network_summary(grp, shared_nodes = NA),
               class = "cograph_bad_parameter")
  expect_error(centrality(grp, shared_nodes = "yes"),
               class = "cograph_bad_parameter")
})

test_that("one-network functions refuse a group instead of pooling it", {
  grp <- group_reply_net()
  expect_error(centrality_degree(grp), class = "cograph_group_input")
  expect_error(reaching_global(grp), class = "cograph_group_input")
  expect_error(plot_centrality(grp), class = "cograph_group_input")
  # the direct verbs, also through do.call and lapply, still work
  expect_equal(nrow(do.call(centrality, list(grp, measures = "degree"))), 20)
  expect_equal(nrow(lapply(list(grp), network_summary)[[1]]), 3)
})

# ---- one measure without a value must not cost the whole table -------------

test_that("type = 'all' keeps every column on a network that is mostly isolates", {
  # 8 connected nodes padded with 15 isolates, as one member of a group
  # object is; mcgm has no automatic alpha here (median coreness is zero)
  saved_rng <- cograph:::.save_rng()
  on.exit(cograph:::.restore_rng(saved_rng), add = TRUE)
  set.seed(5)
  core <- matrix(stats::rbinom(64, 1, 0.4), 8)
  diag(core) <- 0
  w <- matrix(0, 23, 23, dimnames = list(paste0("n", 1:23), paste0("n", 1:23)))
  w[1:8, 1:8] <- core

  expect_error(centrality(w, measures = "mcgm"),
               class = "cograph_undefined_index")
  res <- suppressWarnings(centrality(w, type = "all"))
  expect_equal(nrow(res), 23)
  expect_true("leverage_all" %in% names(res))
  expect_true(all(is.na(res$mcgm)))
  expect_true(all(is.finite(res$leverage_all[1:8])))
  # the measures without a value say so, each with the same warning class
  seen <- character(0)
  withCallingHandlers(
    centrality(w, measures = "all"),
    warning = function(cond) {
      seen <<- c(seen, class(cond)[1L])
      invokeRestart("muffleWarning")
    }
  )
  expect_true("cograph_undefined_measure" %in% seen)
})

test_that("type = 'all' survives unsolvable systems and networks without ties", {
  dyad <- matrix(0, 8, 8, dimnames = list(letters[1:8], letters[1:8]))
  dyad["a", "b"] <- dyad["b", "a"] <- 1
  res <- suppressWarnings(centrality(dyad, type = "all"))
  expect_equal(nrow(res), 8)
  expect_true("leverage_all" %in% names(res))

  empty <- matrix(0, 5, 5, dimnames = list(letters[1:5], letters[1:5]))
  expect_error(centrality(empty, measures = "nonbacktracking"),
               class = "cograph_undefined_index")
  res <- suppressWarnings(centrality(empty, type = "all"))
  expect_equal(nrow(res), 5)
  expect_true(all(is.nan(res$leverage_all)))
})
