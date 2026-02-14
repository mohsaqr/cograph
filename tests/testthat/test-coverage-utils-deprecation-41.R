# Tests for utils-deprecation.R - Comprehensive coverage
# Tests all deprecation utility functions and their branches
# Target: 25+ tests for complete coverage of all code paths

# ============================================
# HANDLE_DEPRECATED_PARAM TESTS
# ============================================

test_that("handle_deprecated_param returns new_val when old_val is NULL", {
  result <- cograph:::handle_deprecated_param(
    new_val = "new_value",
    old_val = NULL,
    new_name = "new_param",
    old_name = "old_param"
  )
  expect_equal(result, "new_value")
})
# Test 1

test_that("handle_deprecated_param returns NULL when both are NULL", {
  result <- cograph:::handle_deprecated_param(
    new_val = NULL,
    old_val = NULL,
    new_name = "new_param",
    old_name = "old_param"
  )
  expect_null(result)
})
# Test 2

test_that("handle_deprecated_param warns when old_val is provided", {
  expect_warning(
    cograph:::handle_deprecated_param(
      new_val = NULL,
      old_val = "old_value",
      new_name = "new_param",
      old_name = "old_param"
    ),
    "'old_param' is deprecated, use 'new_param' instead."
  )
})
# Test 3

test_that("handle_deprecated_param returns old_val when new_val is NULL", {
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = NULL,
      old_val = "old_value",
      new_name = "new_param",
      old_name = "old_param"
    )
  )
  expect_equal(result, "old_value")
})
# Test 4

test_that("handle_deprecated_param returns new_val when both are provided", {
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = "new_value",
      old_val = "old_value",
      new_name = "new_param",
      old_name = "old_param"
    )
  )
  expect_equal(result, "new_value")
})
# Test 5

test_that("handle_deprecated_param with new_val_was_set=FALSE returns old_val", {
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = "default_value",
      old_val = "old_value",
      new_name = "new_param",
      old_name = "old_param",
      new_val_was_set = FALSE
    )
  )
  expect_equal(result, "old_value")
})
# Test 6

test_that("handle_deprecated_param with new_val_was_set=TRUE returns new_val", {
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = "explicit_new_value",
      old_val = "old_value",
      new_name = "new_param",
      old_name = "old_param",
      new_val_was_set = TRUE
    )
  )
  expect_equal(result, "explicit_new_value")
})
# Test 7

test_that("handle_deprecated_param with new_val_was_set=FALSE and old_val=NULL returns new_val", {
  result <- cograph:::handle_deprecated_param(
    new_val = "default_value",
    old_val = NULL,
    new_name = "new_param",
    old_name = "old_param",
    new_val_was_set = FALSE
  )
  expect_equal(result, "default_value")
})
# Test 8

test_that("handle_deprecated_param works with numeric values", {
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = NULL,
      old_val = 42,
      new_name = "new_size",
      old_name = "old_size"
    )
  )
  expect_equal(result, 42)
})
# Test 9

test_that("handle_deprecated_param works with logical values", {
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = NULL,
      old_val = TRUE,
      new_name = "new_flag",
      old_name = "old_flag"
    )
  )
  expect_true(result)
})
# Test 10

test_that("handle_deprecated_param works with vector values", {
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = NULL,
      old_val = c(1, 2, 3),
      new_name = "new_vec",
      old_name = "old_vec"
    )
  )
  expect_equal(result, c(1, 2, 3))
})
# Test 11

test_that("handle_deprecated_param warning message format is correct", {
  expect_warning(
    cograph:::handle_deprecated_param(
      new_val = NULL,
      old_val = "value",
      new_name = "edge_size",
      old_name = "esize"
    ),
    "'esize' is deprecated, use 'edge_size' instead."
  )
})
# Test 12

test_that("handle_deprecated_param with both NULL and new_val_was_set=TRUE returns NULL", {
  result <- cograph:::handle_deprecated_param(
    new_val = NULL,
    old_val = NULL,
    new_name = "new_param",
    old_name = "old_param",
    new_val_was_set = TRUE
  )
  expect_null(result)
})
# Test 13

test_that("handle_deprecated_param preserves list values", {
  test_list <- list(a = 1, b = "test")
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = NULL,
      old_val = test_list,
      new_name = "new_opts",
      old_name = "old_opts"
    )
  )
  expect_equal(result, test_list)
})
# Test 14

test_that("handle_deprecated_param with new_val_was_set=NULL and new_val non-NULL returns new_val", {
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = "new_value",
      old_val = "old_value",
      new_name = "param",
      old_name = "legacy_param",
      new_val_was_set = NULL
    )
  )
  expect_equal(result, "new_value")
})
# Test 15

# ============================================
# FONTFACE_TO_NUMERIC TESTS
# ============================================

test_that("fontface_to_numeric returns input when already numeric", {
  expect_equal(cograph:::fontface_to_numeric(1), 1)
  expect_equal(cograph:::fontface_to_numeric(2), 2)
  expect_equal(cograph:::fontface_to_numeric(3), 3)
  expect_equal(cograph:::fontface_to_numeric(4), 4)
})
# Test 16

test_that("fontface_to_numeric converts 'plain' to 1", {
  result <- cograph:::fontface_to_numeric("plain")
  expect_equal(result, 1)
})
# Test 17

test_that("fontface_to_numeric converts 'bold' to 2", {
  result <- cograph:::fontface_to_numeric("bold")
  expect_equal(result, 2)
})
# Test 18

test_that("fontface_to_numeric converts 'italic' to 3", {
  result <- cograph:::fontface_to_numeric("italic")
  expect_equal(result, 3)
})
# Test 19

test_that("fontface_to_numeric converts 'bold.italic' to 4", {
  result <- cograph:::fontface_to_numeric("bold.italic")
  expect_equal(result, 4)
})
# Test 20

test_that("fontface_to_numeric returns 1 (plain) for unknown string", {
  result <- cograph:::fontface_to_numeric("unknown_fontface")
  expect_equal(result, 1)
})
# Test 21

test_that("fontface_to_numeric handles empty string as default", {
  result <- cograph:::fontface_to_numeric("")
  expect_equal(result, 1)  # defaults to plain
})
# Test 22

test_that("fontface_to_numeric preserves numeric values outside standard range", {
  expect_equal(cograph:::fontface_to_numeric(5), 5)
  expect_equal(cograph:::fontface_to_numeric(0), 0)
})
# Test 23

# ============================================
# FONTFACE_TO_STRING TESTS
# ============================================

test_that("fontface_to_string returns input when already character", {
  expect_equal(cograph:::fontface_to_string("plain"), "plain")
  expect_equal(cograph:::fontface_to_string("bold"), "bold")
  expect_equal(cograph:::fontface_to_string("italic"), "italic")
  expect_equal(cograph:::fontface_to_string("bold.italic"), "bold.italic")
})
# Test 24

test_that("fontface_to_string converts 1 to 'plain'", {
  result <- cograph:::fontface_to_string(1)
  expect_equal(result, "plain")
})
# Test 25

test_that("fontface_to_string converts 2 to 'bold'", {
  result <- cograph:::fontface_to_string(2)
  expect_equal(result, "bold")
})
# Test 26

test_that("fontface_to_string converts 3 to 'italic'", {
  result <- cograph:::fontface_to_string(3)
  expect_equal(result, "italic")
})
# Test 27

test_that("fontface_to_string converts 4 to 'bold.italic'", {
  result <- cograph:::fontface_to_string(4)
  expect_equal(result, "bold.italic")
})
# Test 28

test_that("fontface_to_string returns 'plain' for unknown numeric", {
  result <- cograph:::fontface_to_string(5)
  expect_equal(result, "plain")
})
# Test 29

test_that("fontface_to_string returns 'plain' for 0", {
  result <- cograph:::fontface_to_string(0)
  expect_equal(result, "plain")
})
# Test 30

test_that("fontface_to_string returns 'plain' for negative numeric", {
  result <- cograph:::fontface_to_string(-1)
  expect_equal(result, "plain")
})
# Test 31

test_that("fontface_to_string preserves arbitrary character values", {
  result <- cograph:::fontface_to_string("custom_font")
  expect_equal(result, "custom_font")
})
# Test 32

# ============================================
# ROUND-TRIP CONVERSION TESTS
# ============================================

test_that("fontface numeric -> string -> numeric round-trip preserves value", {
  for (i in 1:4) {
    string_val <- cograph:::fontface_to_string(i)
    numeric_val <- cograph:::fontface_to_numeric(string_val)
    expect_equal(numeric_val, i, info = paste("Failed for", i))
  }
})
# Test 33

test_that("fontface string -> numeric -> string round-trip preserves value", {
  faces <- c("plain", "bold", "italic", "bold.italic")
  for (face in faces) {
    numeric_val <- cograph:::fontface_to_numeric(face)
    string_val <- cograph:::fontface_to_string(numeric_val)
    expect_equal(string_val, face, info = paste("Failed for", face))
  }
})
# Test 34

# ============================================
# EDGE CASES AND SPECIAL VALUES
# ============================================

test_that("handle_deprecated_param works with NA values", {
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = NULL,
      old_val = NA,
      new_name = "new_param",
      old_name = "old_param"
    )
  )
  expect_true(is.na(result))
})
# Test 35

test_that("handle_deprecated_param works with function values", {
  test_fn <- function(x) x + 1
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = NULL,
      old_val = test_fn,
      new_name = "new_func",
      old_name = "old_func"
    )
  )
  expect_true(is.function(result))
  expect_equal(result(5), 6)
})
# Test 36

test_that("handle_deprecated_param with empty string as old_val warns", {
  expect_warning(
    cograph:::handle_deprecated_param(
      new_val = NULL,
      old_val = "",
      new_name = "new_str",
      old_name = "old_str"
    )
  )
})
# Test 37

test_that("handle_deprecated_param with zero as old_val warns and returns zero", {
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = NULL,
      old_val = 0,
      new_name = "new_num",
      old_name = "old_num"
    )
  )
  expect_equal(result, 0)
})
# Test 38

test_that("handle_deprecated_param with FALSE as old_val warns and returns FALSE", {
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = NULL,
      old_val = FALSE,
      new_name = "new_flag",
      old_name = "old_flag"
    )
  )
  expect_false(result)
})
# Test 39

test_that("fontface_to_numeric handles decimal numeric input", {
  result <- cograph:::fontface_to_numeric(2.5)
  expect_equal(result, 2.5)
})
# Test 40

test_that("fontface_to_string handles large numeric input", {
  result <- cograph:::fontface_to_string(100)
  expect_equal(result, "plain")
})
# Test 41

# ============================================
# INTEGRATION-LIKE TESTS
# ============================================

test_that("deprecation flow works for edge_size/esize pattern", {
  # Simulates common usage: new param not set, old param provided
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = NULL,
      old_val = 2.5,
      new_name = "edge_size",
      old_name = "esize"
    )
  )
  expect_equal(result, 2.5)
})
# Test 42

test_that("deprecation flow works for color parameter pattern", {
  # new_val has default, old_val provided
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = "#2E7D32",  # default
      old_val = "#FF0000",  # user-provided old
      new_name = "edge_positive_color",
      old_name = "positive_color",
      new_val_was_set = FALSE
    )
  )
  expect_equal(result, "#FF0000")
})
# Test 43

test_that("deprecation flow respects explicit new param even with old", {
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = "#00FF00",  # explicitly set by user
      old_val = "#FF0000",  # also provided (conflict)
      new_name = "edge_positive_color",
      old_name = "positive_color",
      new_val_was_set = TRUE
    )
  )
  expect_equal(result, "#00FF00")
})
# Test 44

test_that("fontface conversion works for label styling workflow", {
  # User provides string, function converts to numeric for graphics
  user_input <- "bold"
  numeric_face <- cograph:::fontface_to_numeric(user_input)
  expect_equal(numeric_face, 2)

  # Can convert back if needed
  string_face <- cograph:::fontface_to_string(numeric_face)
  expect_equal(string_face, "bold")
})
# Test 45

test_that("handle_deprecated_param with multiple warnings pattern", {
  # Test that multiple deprecated params can be handled sequentially
  w1 <- NULL
  w2 <- NULL

  tryCatch({
    cograph:::handle_deprecated_param(NULL, "val1", "new1", "old1")
  }, warning = function(w) {
    w1 <<- conditionMessage(w)
  })

  tryCatch({
    cograph:::handle_deprecated_param(NULL, "val2", "new2", "old2")
  }, warning = function(w) {
    w2 <<- conditionMessage(w)
  })

  expect_true(grepl("old1", w1))
  expect_true(grepl("old2", w2))
})
# Test 46

test_that("fontface_to_numeric handles integer vs double numeric", {
  # R treats 2L (integer) and 2 (double) differently
  expect_equal(cograph:::fontface_to_numeric(2L), 2L)
  expect_equal(cograph:::fontface_to_numeric(2.0), 2.0)
})
# Test 47

test_that("handle_deprecated_param preserves class of complex objects", {
  test_df <- data.frame(a = 1:3, b = letters[1:3])
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = NULL,
      old_val = test_df,
      new_name = "new_data",
      old_name = "old_data"
    )
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})
# Test 48

test_that("handle_deprecated_param with new_val_was_set handles edge case of NULL new_val", {
  # When new_val_was_set is TRUE but new_val is actually NULL
  # This represents explicit passing of NULL
  result <- suppressWarnings(
    cograph:::handle_deprecated_param(
      new_val = NULL,
      old_val = "old_value",
      new_name = "new_param",
      old_name = "old_param",
      new_val_was_set = TRUE
    )
  )
  # When new_val_was_set=TRUE, we use new_val (even if NULL)
  expect_null(result)
})
# Test 49

test_that("fontface_to_string handles character coercion edge cases", {
  # as.character is called on the fontface inside the function
  result <- cograph:::fontface_to_string(1L)  # integer
  expect_equal(result, "plain")
})
# Test 50
