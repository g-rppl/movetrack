# movetrack

<!-- badges: start -->
[![R-CMD-check](https://github.com/g-rppl/movetrack/workflows/R-CMD-check/badge.svg)](https://github.com/g-rppl/movetrack/actions)
[![codecov](https://codecov.io/gh/g-rppl/movetrack/branch/main/graph/badge.svg)](https://app.codecov.io/gh/g-rppl/movetrack)
[![Universe](https://g-rppl.r-universe.dev/badges/movetrack)](https://g-rppl.r-universe.dev/movetrack)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://github.com/g-rppl/movetrack/blob/main/LICENSE)
<!-- badges: end -->

`movetrack` is an `R` package that provides simple functionality to estimate individual flight tracks from radio-telemetry data such as [Motus](https://motus.org/) using random walk models written in [Stan](https://mc-stan.org/).

## Installation

```r
install.packages("movetrack", repos = "https://g-rppl.r-universe.dev")
```

You can install the development version from GitHub using the `devtools` package:

```r
devtools::install_github("g-rppl/movetrack")
```

During the initial installation, make sure that the C++ toolchain required for `CmdStan` is set up properly. You can find more information [here](https://mc-stan.org/cmdstanr/articles/cmdstanr.html).

```r
library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE)
```

If not, go to <https://mc-stan.org/docs/cmdstan-guide/cmdstan-installation.html#cpp-toolchain> and follow the instructions for your platform. Once your toolchain is configured correctly `CmdStan` can be installed:

```r
install_cmdstan(cores = 2)
```

## Details

This package provides two main functions: `locate()` and `track()`. The first function calculates location estimates based on antenna bearing and signal strength. The second function estimates individual flight paths based on the estimated locations using a Hidden Markov Model written in [Stan](https://mc-stan.org/).

## Getting started
    
You can find a quickstart example in the vignette [movetrack_example](https://g-rppl.github.io/movetrack/articles/movetrack_example.html).

## References

Auger‐Méthé, M., Newman, K., Cole, D., Empacher, F., Gryba, R., King, A. A., ... & Thomas, L. (2021). A guide to state–space modeling of ecological time series. *Ecological Monographs*, 91(4), e01470. doi:[10.1002/ecm.1470](https://doi.org/10.1002/ecm.1470)

Baldwin, J. W., Leap, K., Finn, J. T., & Smetzer, J. R. (2018). Bayesian state-space models reveal unobserved off-shore nocturnal migration from Motus data. *Ecological Modelling*, 386, 38-46. doi:[10.1016/j.ecolmodel.2018.08.006](https://doi.org/10.1016/j.ecolmodel.2018.08.006)

Jonsen, I. D., Flemming, J. M., & Myers, R. A. (2005). Robust state–space modeling of animal movement data. *Ecology*, 86(11), 2874-2880. doi:[10.1890/04-1852](https://doi.org/10.1890/04-1852)
