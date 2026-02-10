# Estimation & Validation Functions (Aside)

These functions require raw data and estimation capabilities.
They are kept here for future integration but are NOT part of the main package.

## Contents

### estimation/
- `bootstrap.R` - Bootstrap resampling for edge significance
- `permutation.R` - Permutation testing for network comparison
- `stability.R` - Centrality stability (CS-coefficient)
- `disparity.R` - Disparity filter for backbone extraction
- `validation-utils.R` - Supporting infrastructure

### tests/
- Related test files for the above functions

## To Restore

To bring these back into the package:
1. Copy files from `aside/estimation/` to `R/`
2. Copy tests from `aside/tests/` to `tests/testthat/`
3. Run `devtools::document()` to update NAMESPACE
4. Run `devtools::test()` to verify

## Dependencies

These functions require:
- Raw sequence data (not just weight matrices)
- The `tna` package for estimation
- `set_raw_data()` to attach data to networks
