# Plotting TNA Models with splot

## 1. Single TNA Model — group_regulation

Build a TNA model from the `group_regulation` dataset (2000 sequences, 9
states).

``` r

data(group_regulation)
model <- tna(group_regulation)
model
#> State Labels : 
#> 
#>    adapt, cohesion, consensus, coregulate, discuss, emotion, monitor, plan, synthesis 
#> 
#> Transition Probability Matrix :
#> 
#>                   adapt   cohesion  consensus coregulate    discuss    emotion
#> adapt      0.0000000000 0.27308448 0.47740668 0.02161100 0.05893910 0.11984283
#> cohesion   0.0029498525 0.02713864 0.49793510 0.11917404 0.05958702 0.11563422
#> consensus  0.0047400853 0.01485227 0.08200348 0.18770738 0.18802338 0.07268131
#> coregulate 0.0162436548 0.03604061 0.13451777 0.02335025 0.27360406 0.17208122
#> discuss    0.0713743356 0.04758289 0.32118451 0.08428246 0.19488737 0.10579600
#> emotion    0.0024673951 0.32534367 0.32040888 0.03419105 0.10186817 0.07684173
#> monitor    0.0111653873 0.05582694 0.15910677 0.05792045 0.37543615 0.09071877
#> plan       0.0009745006 0.02517460 0.29040117 0.01721618 0.06789021 0.14682475
#> synthesis  0.2346625767 0.03374233 0.46625767 0.04447853 0.06288344 0.07055215
#>               monitor       plan   synthesis
#> adapt      0.03339882 0.01571709 0.000000000
#> cohesion   0.03303835 0.14100295 0.003539823
#> consensus  0.04661084 0.39579712 0.007584137
#> coregulate 0.08629442 0.23908629 0.018781726
#> discuss    0.02227284 0.01164262 0.140976968
#> emotion    0.03630596 0.09975326 0.002819880
#> monitor    0.01814375 0.21563154 0.016050244
#> plan       0.07552379 0.37420822 0.001786584
#> synthesis  0.01226994 0.07515337 0.000000000
#> 
#> Initial Probabilities : 
#> 
#>      adapt   cohesion  consensus coregulate    discuss    emotion    monitor 
#>     0.0115     0.0605     0.2140     0.0190     0.1755     0.1515     0.1440 
#>       plan  synthesis 
#>     0.2045     0.0195
```

### Basic Network Plot

``` r

splot(model,
      title = "Group Regulation TNA",
      minimum = 0.05)
```

![](plotting-tna-models_files/figure-html/plot-basic-1.png)

## 2. Bootstrap Analysis

Run bootstrap resampling (1000 iterations) to assess edge significance
and confidence intervals.

``` r

set.seed(42)
boot <- bootstrap(model, iter = 1000)
```

``` r

sig_edges <- boot$summary[boot$summary$sig, ]
cat(sprintf("Significant edges: %d / %d\n", nrow(sig_edges), nrow(boot$summary)))
#> Significant edges: 51 / 78
head(sig_edges[order(sig_edges$p_value), ], 10)
#>          from        to     weight     p_value  sig   cr_lower   cr_upper
#> 5     discuss     adapt 0.07137434 0.000999001 TRUE 0.05353075 0.08921792
#> 9   synthesis     adapt 0.23466258 0.000999001 TRUE 0.17599693 0.29332822
#> 10      adapt  cohesion 0.27308448 0.000999001 TRUE 0.20481336 0.34135560
#> 14    discuss  cohesion 0.04758289 0.000999001 TRUE 0.03568717 0.05947861
#> 15    emotion  cohesion 0.32534367 0.000999001 TRUE 0.24400775 0.40667959
#> 17       plan  cohesion 0.02517460 0.000999001 TRUE 0.01888095 0.03146825
#> 19      adapt consensus 0.47740668 0.000999001 TRUE 0.35805501 0.59675835
#> 20   cohesion consensus 0.49793510 0.000999001 TRUE 0.37345133 0.62241888
#> 21  consensus consensus 0.08200348 0.000999001 TRUE 0.06150261 0.10250435
#> 22 coregulate consensus 0.13451777 0.000999001 TRUE 0.10088832 0.16814721
#>      ci_lower   ci_upper
#> 5  0.06380715 0.08001081
#> 9  0.20156075 0.26888806
#> 10 0.23990971 0.31203056
#> 14 0.04110513 0.05353159
#> 15 0.30902433 0.34318567
#> 17 0.02145591 0.02934643
#> 19 0.43274713 0.51731885
#> 20 0.47293205 0.52218913
#> 21 0.07575667 0.08881174
#> 22 0.11951905 0.14968376
```

### Bootstrap — Significant Edges Only

``` r

splot(boot,
      display = "significant",
      title = "Bootstrap — Significant Edges",
      show_stars = TRUE)
```

![](plotting-tna-models_files/figure-html/plot-boot-sig-1.png)

### Bootstrap — Full Network with Styling

Non-significant edges shown as dashed gray lines.

``` r

splot(boot,
      display = "styled",
      title = "Bootstrap — Styled (sig=solid, nonsig=dashed)",
      show_stars = TRUE,
      threshold  = 0.02)
```

![](plotting-tna-models_files/figure-html/plot-boot-styled-1.png)

### Bootstrap — Confidence Intervals

``` r

splot(boot,
      display = "ci",
      title = "Bootstrap — With Confidence Intervals",
      show_ci = TRUE,
      minimum = 0.05)
```

![](plotting-tna-models_files/figure-html/plot-boot-ci-1.png)

## 3. Simulated Group TNA + Permutation Test

Generate two group TNA networks then compare with permutation testing.

``` r

set.seed(123)
group_models <- group_tna(group_regulation, group = c(rep("H",1000),rep("L",1000)))
```

### Plot Each Group

``` r

par(mfrow = c(1, 2))
splot(group_models[[1]],
      title = "Group 1",
      minimum = 0.05)
splot(group_models[[2]],
      title = "Group 2",
      minimum = 0.05)
```

![](plotting-tna-models_files/figure-html/plot-groups-1.png)

``` r

par(mfrow = c(1, 1))
```

### Difference Network

``` r

cograph::plot_compare(
  group_models[[1]], group_models[[2]],
  title = "Group 1 vs Group 2 — Difference Network")
```

![](plotting-tna-models_files/figure-html/plot-compare-1.png)

### Permutation Test

``` r

set.seed(42)
perm <- tna::permutation_test(group_models[[1]], group_models[[2]], iter = 1000)
```

``` r

cat(capture.output(str(perm, max.level = 1)), sep = "\n")
#> List of 1
#>  $ edges:List of 3
#>  - attr(*, "labels")= chr [1:9] "adapt" "cohesion" "consensus" "coregulate" ...
#>  - attr(*, "colors")= chr [1:9] "#8DD3C7" "#FFFFB3" "#BEBADA" "#FB8072" ...
#>  - attr(*, "class")= chr "tna_permutation"
```

### Permutation Test — Network

``` r

cograph::plot_permutation(perm,
      title = "Permutation Test — Significant Differences",
      show_nonsig = TRUE)
```

![](plotting-tna-models_files/figure-html/plot-perm-1.png)
