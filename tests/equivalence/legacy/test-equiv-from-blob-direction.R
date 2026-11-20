# Equivalence tests moved from tests/testthat/test-blob-direction.R: cograph against
# other implementations. Developer-only; not part of R CMD check.
# The source file's top-level setup is repeated below so every block keeps
# its fixtures and skips.

# ---- setup from the source file ----
# (none)

# ---- equivalence tests ----

test_that(".step_shade matches the JavaScript reference values", {
  # node blue #4A7FB5 at t = 0, 0.5, 1 and target orange #E8734A at t = 0, 1,
  # from simStepShade() in carmnote-tna-pro/viz/carmtna-render.js.
  expect_equal(toupper(.step_shade("#4A7FB5", c(0, 0.5, 1))),
               c("#BACEE3", "#789ABD", "#3F6C9A"))
  expect_equal(toupper(.step_shade("#E8734A", c(0, 1))),
               c("#F6CABA", "#C5623F"))
})
