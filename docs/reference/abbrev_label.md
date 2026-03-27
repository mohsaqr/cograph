# Abbreviate Labels

Abbreviates labels to a maximum length, adding ellipsis if truncated.

## Usage

``` r
abbrev_label(label, abbrev = NULL, n_labels = NULL)

label_abbrev(label, abbrev = NULL, n_labels = NULL)
```

## Arguments

- label:

  Character vector of labels to abbreviate.

- abbrev:

  Abbreviation control:

  - NULL: No abbreviation (return labels unchanged)

  - Integer: Maximum character length (truncate + ellipsis)

  - "auto": Adaptive abbreviation based on label count

- n_labels:

  Number of labels (used for "auto" mode). If NULL, uses length(label).

## Value

Character vector of (possibly abbreviated) labels.

## Examples

``` r
labels <- c("VeryLongStateName", "Short", "AnotherLongName")

# No abbreviation
abbrev_label(labels, NULL)
#> [1] "VeryLongStateName" "Short"             "AnotherLongName"  

# Fixed max length
abbrev_label(labels, 5)  # "Very...", "Short", "Anot..."
#> [1] "Very…" "Short" "Anot…"

# Auto-adaptive
abbrev_label(labels, "auto")
#> [1] "VeryLongStateName" "Short"             "AnotherLongName"  
```
