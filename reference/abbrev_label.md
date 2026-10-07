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

  - "auto": Length chosen from the label count. Up to 5 labels are kept
    whole, and up to 8, 12, 20 or more labels are cut to 15, 10, 6 or 4
    characters.

- n_labels:

  Number of labels (used for "auto" mode). If NULL, uses length(label).

## Value

Character vector of (possibly abbreviated) labels.

## Examples

``` r
abbrev_label(colnames(regulation_net), abbrev = 4)
#>  [1] "Exp…" "Plan" "Mon…" "Ada…" "Ref…" "Dis…" "Syn…" "Eva…" "Cre…" "Sha…"
```
