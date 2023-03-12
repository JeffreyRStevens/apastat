
<!-- README.md is generated from README.Rmd. Please edit that file -->

# apastat

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of apastat is to provide functions that format statistical
output in a way that can be inserted into R Markdown documents. This is
analogous to the
[`apa_print()`](https://frederikaust.com/papaja_man/reporting.html#statistical-models-and-tests)
functions in the [papaja](https://github.com/crsh/papaja) package but
prints Markdown syntax instead of LaTeX. The defaults follow American
Psychological Association style, but some defaults can be over-ridden.

## Installation

You can install the development version of apastat from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("JeffreyRStevens/apastat")
```

## Example

For an example, we’ll create a data set from the `beavers1` and
`beavers2` data sets.

``` r
library(apastat)
beavers <- merge(beaver1, beaver2, by = "time")
beavers <- beavers[, c("time", "temp.x", "temp.y")]
beavers_corr <- cor.test(beavers$temp.x, beavers$temp.y)
```

Now we can apply the `apa_ttest()` to `beavers_corr` to create a
Markdown- formatted character string for the statistical results. We can
embed this as inline R Markdown code to generate the results.

#### Code

`` `The temperature for the two beavers was highly correlated (`r apa_ttest(beavers_corr, digits = 2)`). ``

#### Output

The temperature for the two beavers was highly correlated (Mean = 0.42,
95% CI \[0.24, 0.57\], *t*(97) = 4.52, *p* \< .001).

## Citation

To cite apastat, use:

> Stevens, Jeffrey R. (2023). apastat: Format and print statistical
> output for Markdown. (version 0.0.0.9000)
> <https://github.com/JeffreyRStevens/apastat>
