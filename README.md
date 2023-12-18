# motusTrack <img src="https://raw.githubusercontent.com/stan-dev/logos/master/logo_tm.png" align="right" width="120" />

[![R-CMD-check](https://github.com/g-rppl/motusTrack/workflows/R-CMD-check/badge.svg)](https://github.com/g-rppl/motusTrack/actions)
[![codecov](https://codecov.io/gh/g-rppl/motusTrack/branch/main/graph/badge.svg)](https://app.codecov.io/gh/g-rppl/motusTrack)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://github.com/g-rppl/motusTrack/blob/main/LICENSE)

`motusTrack` is a `R` package that provides simple functionality to estimate flight tracks from radio-telemetry data such as [Motus](https://motus.org/) using random walk models written in [Stan](https://mc-stan.org/).

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

Now you can install `motusTrack` using:

```r
library(devtools)
install_github("g-rppl/motusTrack", ref = "main", dependencies = TRUE,
auth_token = "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")
```

## Example workflow
    
```r
library(motusTrack)
library(tidyverse)
library(leaflet)

# load data
data(motusData)

# estimate locations based on antenna bearings and signal strength
loc <- locate(motusData, dtime = 2)

# model flight path
fit <- track(loc, parallel_chains = 4, refresh = 1e3)

# plot flight path
leaflet() %>%
    addTiles() %>%
    addCircles(
        lng = ~mean.lon, lat = ~mean.lat,
        data = fit[fit$ID == 49237, ]
    ) %>%
    addCircles(
        lng = ~mean.lon, lat = ~mean.lat,
        data = fit[fit$ID == 50893, ], color = "orange"
    ) %>%
    addPolylines(
        lng = ~mean.lon, lat = ~mean.lat,
        data = fit[fit$ID == 49237, ]
    ) %>%
    addPolylines(
        lng = ~mean.lon, lat = ~mean.lat,
        data = fit[fit$ID == 50893, ], color = "orange"
    ) %>%
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