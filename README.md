# stantrackr

[![R-CMD-check](https://github.com/g-rppl/stantrackr/workflows/R-CMD-check/badge.svg)](https://github.com/g-rppl/stantrackr/actions)
[![codecov](https://codecov.io/gh/g-rppl/stantrackr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/g-rppl/stantrackr)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://github.com/g-rppl/stantrackr/blob/main/LICENSE)

`stantrackr` is a `R` package that provides simple functionality to estimate individual flight tracks from radio-telemetry data such as [Motus](https://motus.org/) using random walk models written in [Stan](https://mc-stan.org/).

## Installation

First make sure that `cmdstanr` and `CmdStan` are available on your system. You can find more information [here](https://mc-stan.org/cmdstanr/articles/cmdstanr.html).

```r
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
```

Check whether there is a suitable C++ toolchain installed on your system:

```r
library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE)
```

If not, go to <https://mc-stan.org/docs/cmdstan-guide/cmdstan-installation.html#cpp-toolchain> and follow the instructions for your platform. Once your toolchain is configured correctly `CmdStan` can be installed:

```r
install_cmdstan(cores = 2)
```

Now you can install `stantrackr` using:

```r
library(devtools)
install_github("g-rppl/stantrackr")
```

## Example workflow
    
```r
library(motusTrack)
library(tidyverse)
library(sfheaders)
library(leaflet)

# load data
data(motusData)

# estimate locations based on antenna bearings and signal strength
loc <- locate(motusData, dtime = 2)

# model flight paths
fit <- track(loc, parallel_chains = 4, refresh = 1e3)

# plot flight paths
fit %>%
    sf_linestring("mean.lon", "mean.lat", linestring_id = "ID") %>%
    leaflet() %>%
    addTiles() %>%
    addPolylines(color = ~c("orange", "blue")) %>%
    addCircles(
        lng = ~recvDeployLon,
        lat = ~recvDeployLat,
        data = motusData,
        color = "black",
        opacity = 1
    )

# plot flight speed
ggplot(fit) +
    geom_path(aes(
        x = mean.lon, y = mean.lat,
        group = ID, colour = speed
    )) +
    scale_color_viridis_c()
```

## Details

This package provides two main functions: `locate()` and `track()`. The first function calculates location estimates based on antenna bearing and signal strength. The second function estimates individual flight paths based on the estimated locations using random walk models written in [Stan](https://mc-stan.org/).

## References

Auger‐Méthé, M., Newman, K., Cole, D., Empacher, F., Gryba, R., King, A. A., ... & Thomas, L. (2021). A guide to state–space modeling of ecological time series. *Ecological Monographs*, 91(4), e01470. doi: [10.1002/ecm.1470](https://doi.org/10.1002/ecm.1470)

Baldwin, J. W., Leap, K., Finn, J. T., & Smetzer, J. R. (2018). Bayesian state-space models reveal unobserved off-shore nocturnal migration from Motus data. *Ecological Modelling*, 386, 38-46. doi: [10.1016/j.ecolmodel.2018.08.006](https://doi.org/10.1016/j.ecolmodel.2018.08.006)

Jonsen, I. D., Flemming, J. M., & Myers, R. A. (2005). Robust state–space modeling of animal movement data. *Ecology*, 86(11), 2874-2880. doi: [10.1890/04-1852](https://doi.org/10.1890/04-1852)
