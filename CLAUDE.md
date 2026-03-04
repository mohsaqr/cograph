# cograph Project Configuration

## Environment

- **Platform**: macOS (Darwin)
- **Rscript**: Available on PATH (`Rscript`)
- **R version**: 4.5+

## Running R Scripts

```bash
Rscript script_name.R
```

## Project Overview

cograph is an R package for modern network visualization. Key functions:
- `splot()` - Base R graphics network plotting
- `soplot()` - Grid/ggplot2-style network plotting
- `plot_compare()` - Difference networks
- `plot_chord()` - Chord diagrams
- `plot_time_line()` - Cluster timeline visualization
- `plot_bootstrap()` - Bootstrap result visualization
- `plot_permutation()` - Permutation test visualization
- `plot_alluvial()` / `plot_trajectories()` - Transition flow plots

## Common Commands

```bash
# Load and check package
Rscript -e 'devtools::load_all(".")'

# Run all tests
Rscript -e 'devtools::test(".")'

# Run a single test file
Rscript -e 'testthat::test_file("tests/testthat/test-plot-timeline.R")'

# Build documentation
Rscript -e 'devtools::document(".")'

# R CMD check (no tests/examples for quick check)
Rscript -e 'devtools::check(".", args = "--no-tests --no-examples --no-vignettes --no-manual")'

# Install package locally
Rscript -e 'devtools::install(".", upgrade = "never")'
```
