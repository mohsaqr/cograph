# Plot Group Permutation Test Results

Visualizes all pairwise permutation test results from a group_tna
object. Creates a multi-panel plot with one panel per comparison.

## Usage

``` r
splot.group_tna_permutation(x, ...)

plot_group_permutation(x, i = NULL, ...)
```

## Arguments

- x:

  A group_tna_permutation object (from tna::permutation_test on
  group_tna).

- ...:

  Additional arguments passed to plot_permutation().

- i:

  Index or name of specific comparison to plot. NULL for all.

## Value

Invisibly returns NULL.

## Examples

``` r
# Mock a group_tna_permutation object
d1 <- matrix(c(0, .2, -.1, -.2, 0, .1, .1, -.1, 0), 3, 3)
rownames(d1) <- colnames(d1) <- c("A", "B", "C")
d1_sig <- d1; d1_sig[abs(d1) < 0.15] <- 0
perm1 <- list(edges = list(diffs_true = d1, diffs_sig = d1_sig, stats = NULL))
attr(perm1, "labels") <- c("A", "B", "C")
class(perm1) <- c("tna_permutation", "list")
gperm <- list("G1 vs. G2" = perm1)
class(gperm) <- c("group_tna_permutation", "list")
plot_group_permutation(gperm)

```
