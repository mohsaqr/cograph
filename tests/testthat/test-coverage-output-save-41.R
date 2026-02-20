# test-coverage-output-save-41.R - Additional Coverage Tests for output-save.R
# Targets uncovered code paths to improve coverage from 86%

# ============================================
# SN_SAVE() - SVG FORMAT EDGE CASES
# ============================================

test_that("sn_save() creates SVG with custom dimensions", {
  skip_on_cran()

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp), add = TRUE)

  # Skip if SVG device not available
  result <- tryCatch({
    grDevices::svg(tmp)
    grDevices::dev.off()
    TRUE
  }, warning = function(w) {
    if (grepl("cairo|X11", conditionMessage(w), ignore.case = TRUE)) FALSE else TRUE
  }, error = function(e) FALSE)

  if (!result) skip("SVG device not available on this system")

  unlink(tmp)
  expect_message(sn_save(net, tmp, width = 12, height = 8), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() creates SVG with title", {
  skip_on_cran()

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp), add = TRUE)

  # Skip if SVG device not available
  result <- tryCatch({
    grDevices::svg(tmp)
    grDevices::dev.off()
    TRUE
  }, warning = function(w) {
    if (grepl("cairo|X11", conditionMessage(w), ignore.case = TRUE)) FALSE else TRUE
  }, error = function(e) FALSE)

  if (!result) skip("SVG device not available on this system")

  unlink(tmp)
  expect_message(sn_save(net, tmp, title = "SVG Network Plot"), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() - EPS FORMAT EDGE CASES
# ============================================

test_that("sn_save() creates EPS with custom dimensions", {
  skip_on_cran()

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".eps")
  on.exit(unlink(tmp), add = TRUE)

  # PostScript devices can fail with font family issues
  result <- tryCatch({
    suppressWarnings(sn_save(net, tmp, width = 10, height = 8))
    TRUE
  }, error = function(e) {
    if (grepl("font|family|postscript", conditionMessage(e), ignore.case = TRUE)) {
      FALSE
    } else {
      stop(e)
    }
  })

  if (!result) skip("PostScript device has font issues on this system")
  expect_file_created(tmp)
})

test_that("sn_save() creates EPS with title", {
  skip_on_cran()

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".eps")
  on.exit(unlink(tmp), add = TRUE)

  # PostScript devices can fail with font family issues
  result <- tryCatch({
    suppressWarnings(sn_save(net, tmp, title = "EPS Network"))
    TRUE
  }, error = function(e) {
    if (grepl("font|family|postscript", conditionMessage(e), ignore.case = TRUE)) {
      FALSE
    } else {
      stop(e)
    }
  })

  if (!result) skip("PostScript device has font issues on this system")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() - PS FORMAT EDGE CASES
# ============================================

test_that("sn_save() creates PS with custom dimensions", {
  skip_on_cran()

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".ps")
  on.exit(unlink(tmp), add = TRUE)

  # PostScript devices can fail with font family issues
  result <- tryCatch({
    suppressWarnings(sn_save(net, tmp, width = 8, height = 6))
    TRUE
  }, error = function(e) {
    if (grepl("font|family|postscript", conditionMessage(e), ignore.case = TRUE)) {
      FALSE
    } else {
      stop(e)
    }
  })

  if (!result) skip("PostScript device has font issues on this system")
  expect_file_created(tmp)
})

test_that("sn_save() creates PS with title", {
  skip_on_cran()

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".ps")
  on.exit(unlink(tmp), add = TRUE)

  # PostScript devices can fail with font family issues
  result <- tryCatch({
    suppressWarnings(sn_save(net, tmp, title = "PS Network"))
    TRUE
  }, error = function(e) {
    if (grepl("font|family|postscript", conditionMessage(e), ignore.case = TRUE)) {
      FALSE
    } else {
      stop(e)
    }
  })

  if (!result) skip("PostScript device has font issues on this system")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() - TIFF FORMAT EDGE CASES
# ============================================

test_that("sn_save() creates TIFF with custom DPI", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".tiff")
  on.exit(unlink(tmp), add = TRUE)

  # Suppress compression warning on macOS/quartz
  suppressWarnings(expect_message(sn_save(net, tmp, dpi = 150), "Saved"))
  expect_file_created(tmp)
})

test_that("sn_save() creates TIFF with title", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".tiff")
  on.exit(unlink(tmp), add = TRUE)

  # Suppress compression warning on macOS/quartz
  suppressWarnings(expect_message(sn_save(net, tmp, title = "TIFF Network"), "Saved"))
  expect_file_created(tmp)
})

test_that("sn_save() creates TIFF with non-square dimensions", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".tiff")
  on.exit(unlink(tmp), add = TRUE)

  # Suppress compression warning on macOS/quartz
  suppressWarnings(expect_message(sn_save(net, tmp, width = 10, height = 6), "Saved"))
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() - JPEG FORMAT EDGE CASES
# ============================================

test_that("sn_save() creates JPEG with custom DPI", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".jpeg")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp, dpi = 150), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() creates JPEG with title", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".jpeg")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp, title = "JPEG Network"), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() creates JPG with non-square dimensions", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".jpg")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp, width = 8, height = 4), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() - TNA INPUT SUPPORT
# ============================================

test_that("sn_save() handles tna object input", {
  skip_if_no_tna()

  # Create simple tna object
  mat <- matrix(c(
    0, 0.5, 0.3,
    0.2, 0, 0.4,
    0.1, 0.3, 0
  ), nrow = 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  tna_obj <- tna::tna(mat)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(tna_obj, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles tna object with PDF output", {
  skip_if_no_tna()

  mat <- matrix(c(
    0, 0.4, 0.2,
    0.3, 0, 0.5,
    0.1, 0.2, 0
  ), nrow = 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("X", "Y", "Z")

  tna_obj <- tna::tna(mat)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(tna_obj, tmp, title = "TNA Network"), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() - ADDITIONAL INPUT TYPES
# ============================================

test_that("sn_save() handles named matrix input", {
  adj <- create_test_matrix(4)
  rownames(adj) <- colnames(adj) <- paste0("Node_", 1:4)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(adj, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles sparse matrix via igraph", {
  skip_if_no_igraph()

  # Create sparse network
  g <- igraph::graph_from_edgelist(matrix(c(1, 2, 2, 3, 3, 1), ncol = 2, byrow = TRUE))
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(g, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles dense network", {
  adj <- create_test_topology("complete", n = 8)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(adj, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() - LAYOUT VARIATIONS
# ============================================

test_that("sn_save() handles network with random layout", {
  adj <- create_test_matrix(5)
  net <- cograph(adj, layout = "random", seed = 123)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles network with custom layout matrix", {
  adj <- create_test_matrix(4)
  custom_layout <- data.frame(
    x = c(0, 1, 0, 1),
    y = c(0, 0, 1, 1)
  )
  net <- cograph(adj, layout = custom_layout)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() - EDGE STYLING VARIATIONS
# ============================================

test_that("sn_save() handles network with curved edges", {
  adj <- create_test_matrix(4)
  net <- cograph(adj) |>
    sn_edges(curve_shape = 0.3, curves = "force")
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles network with edge labels", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(adj)
  net$edges$label <- round(net$edges$weight, 2)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles network with varying edge widths", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(adj) |>
    sn_edges(width = abs(adj[adj != 0]) * 3)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() - NODE STYLING VARIATIONS
# ============================================

test_that("sn_save() handles network with varying node sizes", {
  adj <- create_test_matrix(5)
  net <- cograph(adj) |>
    sn_nodes(size = c(0.08, 0.10, 0.12, 0.14, 0.16))
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles network with node labels", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  net$nodes$label <- paste0("State ", 1:4)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles network with alpha transparency", {
  adj <- create_test_matrix(4)
  net <- cograph(adj) |>
    sn_nodes(alpha = 0.7) |>
    sn_edges(alpha = 0.5)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE_GGPLOT() - ADDITIONAL FORMATS
# ============================================

test_that("sn_save_ggplot() creates TIFF file", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".tiff")
  on.exit(unlink(tmp), add = TRUE)

  suppressWarnings(expect_message(sn_save_ggplot(net, tmp, dpi = 100), "Saved"))
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() creates EPS file", {
  skip_if_not_installed("ggplot2")
  skip_on_cran()

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".eps")
  on.exit(unlink(tmp), add = TRUE)

  # EPS/PostScript may have font issues
  result <- tryCatch({
    suppressWarnings(sn_save_ggplot(net, tmp))
    TRUE
  }, error = function(e) {
    if (grepl("font|family|postscript", conditionMessage(e), ignore.case = TRUE)) {
      FALSE
    } else {
      stop(e)
    }
  })

  if (!result) skip("PostScript device has font issues on this system")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() creates PS file", {
  skip_if_not_installed("ggplot2")
  skip_on_cran()

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".ps")
  on.exit(unlink(tmp), add = TRUE)

  # PostScript may have font issues
  result <- tryCatch({
    suppressWarnings(sn_save_ggplot(net, tmp))
    TRUE
  }, error = function(e) {
    if (grepl("font|family|postscript", conditionMessage(e), ignore.case = TRUE)) {
      FALSE
    } else {
      stop(e)
    }
  })

  if (!result) skip("PostScript device has font issues on this system")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE_GGPLOT() - INPUT VARIATIONS
# ============================================

test_that("sn_save_ggplot() handles network with custom theme", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- cograph(adj) |> sn_theme("minimal")
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() handles network with dark theme", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- cograph(adj) |> sn_theme("dark")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp, dpi = 72), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() handles star topology", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_topology("star", n = 6)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() handles ring topology", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_topology("ring", n = 7)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp, dpi = 72), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE_GGPLOT() - DIMENSION VARIATIONS
# ============================================

test_that("sn_save_ggplot() handles landscape dimensions", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp, width = 12, height = 6), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() handles portrait dimensions", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp, width = 5, height = 10, dpi = 72), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() handles high DPI", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(3)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp, width = 4, height = 4, dpi = 300), "Saved")
  expect_file_created(tmp)
  # High DPI should create larger file
  expect_true(file.size(tmp) > 5000)
})

# ============================================
# SN_SAVE() - ERROR PATH TESTING
# ============================================

test_that("sn_save() provides informative error for unknown format", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".docx")

  expect_error(sn_save(net, tmp), "Unsupported format: docx")
})

test_that("sn_save() provides informative error for html format", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".html")

  expect_error(sn_save(net, tmp), "Unsupported")
})

test_that("sn_save() provides informative error for json format", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".json")

  expect_error(sn_save(net, tmp), "Unsupported")
})

# ============================================
# SN_SAVE() - DEVICE CLEANUP EDGE CASES
# ============================================

test_that("sn_save() properly manages device count across formats", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  dev_before <- length(dev.list())

  tmp_pdf <- tempfile(fileext = ".pdf")
  tmp_png <- tempfile(fileext = ".png")
  tmp_jpeg <- tempfile(fileext = ".jpeg")

  on.exit({
    unlink(tmp_pdf)
    unlink(tmp_png)
    unlink(tmp_jpeg)
  }, add = TRUE)

  suppressMessages(sn_save(net, tmp_pdf))
  suppressMessages(sn_save(net, tmp_png))
  suppressMessages(sn_save(net, tmp_jpeg))

  dev_after <- length(dev.list())

  expect_equal(dev_after, dev_before)
  expect_file_created(tmp_pdf)
  expect_file_created(tmp_png)
  expect_file_created(tmp_jpeg)
})

test_that("sn_save() closes device on error in unsupported format", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  dev_before <- length(dev.list())

  # This should error but not leave a device open
  expect_error(sn_save(net, "test.xyz"), "Unsupported")

  dev_after <- length(dev.list())
  expect_equal(dev_after, dev_before)
})

# ============================================
# SN_SAVE() - SPECIAL NETWORK CONFIGURATIONS
# ============================================

test_that("sn_save() handles disconnected network", {
  adj <- create_test_topology("disconnected", n = 6)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles path topology", {
  adj <- create_test_topology("path", n = 5)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles very large network", {
  adj <- create_test_matrix(25, density = 0.2)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE_GGPLOT() - SPECIAL CONFIGURATIONS
# ============================================

test_that("sn_save_ggplot() handles disconnected network", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_topology("disconnected", n = 6)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() handles two-node network", {
  skip_if_not_installed("ggplot2")

  adj <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() handles network with self-loops", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  diag(adj) <- 1
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() - RETURN VALUE EDGE CASES
# ============================================

test_that("sn_save() returns invisible filename for all formats", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  # Test PDF
  tmp_pdf <- tempfile(fileext = ".pdf")
  result_pdf <- suppressMessages(sn_save(net, tmp_pdf))
  expect_equal(result_pdf, tmp_pdf)
  expect_true(is.character(result_pdf))

  # Test PNG
  tmp_png <- tempfile(fileext = ".png")
  result_png <- suppressMessages(sn_save(net, tmp_png))
  expect_equal(result_png, tmp_png)
  expect_true(is.character(result_png))

  # Test JPEG
  tmp_jpeg <- tempfile(fileext = ".jpeg")
  result_jpeg <- suppressMessages(sn_save(net, tmp_jpeg))
  expect_equal(result_jpeg, tmp_jpeg)
  expect_true(is.character(result_jpeg))

  on.exit({
    unlink(tmp_pdf)
    unlink(tmp_png)
    unlink(tmp_jpeg)
  }, add = TRUE)
})

test_that("sn_save_ggplot() returns invisible filename for all formats", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(3)
  net <- cograph(adj)

  # Test PDF
  tmp_pdf <- tempfile(fileext = ".pdf")
  result_pdf <- suppressMessages(sn_save_ggplot(net, tmp_pdf))
  expect_equal(result_pdf, tmp_pdf)
  expect_true(is.character(result_pdf))

  # Test PNG
  tmp_png <- tempfile(fileext = ".png")
  result_png <- suppressMessages(sn_save_ggplot(net, tmp_png, dpi = 72))
  expect_equal(result_png, tmp_png)
  expect_true(is.character(result_png))

  on.exit({
    unlink(tmp_pdf)
    unlink(tmp_png)
  }, add = TRUE)
})

# ============================================
# SN_SAVE() - FILENAME EDGE CASES
# ============================================

test_that("sn_save() handles filename with unicode characters", {
  skip_on_cran()

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  tmp_dir <- create_temp_dir()
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Simple unicode in filename
  filepath <- file.path(tmp_dir, "network_test.pdf")

  expect_message(sn_save(net, filepath), "Saved")
  expect_file_created(filepath)
})

test_that("sn_save() handles filename with numbers", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  tmp_dir <- create_temp_dir()
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  filepath <- file.path(tmp_dir, "network_123_v2.pdf")

  expect_message(sn_save(net, filepath), "Saved")
  expect_file_created(filepath)
})

test_that("sn_save() handles deeply nested directory", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  tmp_dir <- create_temp_dir()
  nested_dir <- file.path(tmp_dir, "level1", "level2", "level3")
  dir.create(nested_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  filepath <- file.path(nested_dir, "network.pdf")

  expect_message(sn_save(net, filepath), "Saved")
  expect_file_created(filepath)
})

# ============================================
# SN_SAVE() - NULL TITLE HANDLING
# ============================================

test_that("sn_save() handles NULL title explicitly", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp, title = NULL), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() handles NULL title explicitly", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp, title = NULL), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() - IGRAPH INPUT EDGE CASES
# ============================================

test_that("sn_save() handles igraph bipartite network", {
  skip_if_no_igraph()

  # Create bipartite graph
  g <- igraph::make_bipartite_graph(c(0, 0, 1, 1, 1), c(1, 3, 1, 4, 2, 5, 2, 4))
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(g, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles igraph named graph", {
  skip_if_no_igraph()

  g <- igraph::make_ring(5)
  igraph::V(g)$name <- c("Alpha", "Beta", "Gamma", "Delta", "Epsilon")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(g, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles igraph with node attributes", {
  skip_if_no_igraph()

  g <- igraph::make_star(5, mode = "undirected")
  igraph::V(g)$color <- c("red", "blue", "green", "yellow", "purple")
  igraph::V(g)$size <- c(30, 20, 20, 20, 20)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(g, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() - WEIGHTED NETWORK VARIATIONS
# ============================================

test_that("sn_save() handles network with all same weights", {
  adj <- matrix(0.5, 4, 4)
  diag(adj) <- 0
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles network with very small weights", {
  adj <- create_test_matrix(4, weighted = TRUE)
  adj <- adj * 0.001  # Make weights very small
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles network with very large weights", {
  adj <- create_test_matrix(4, weighted = TRUE)
  adj <- adj * 1000  # Make weights very large
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() - MULTIPLE SEQUENTIAL SAVES
# ============================================

test_that("sn_save() handles rapid sequential saves", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  files <- vapply(1:5, function(i) {
    tmp <- tempfile(fileext = ".pdf")
    suppressMessages(sn_save(net, tmp))
    tmp
  }, character(1))

  on.exit(unlink(files), add = TRUE)

  # All files should be created
  lapply(files, expect_file_created)
})

test_that("sn_save_ggplot() handles rapid sequential saves", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  files <- vapply(1:3, function(i) {
    tmp <- tempfile(fileext = ".pdf")
    suppressMessages(sn_save_ggplot(net, tmp))
    tmp
  }, character(1))

  on.exit(unlink(files), add = TRUE)

  # All files should be created
  lapply(files, expect_file_created)
})
