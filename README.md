# ggcyclical
<!-- badges: start -->
[![R-CMD-check](https://github.com/schwi24/ggcyclical/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/schwi24/ggcyclical/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

R package for tidy analysis of cyclical (circular, periodic, directional, polar) data and plotting with ggplot2.

This package extends the package `ggplot2` with functions to plot cyclical data and uses many statistical tools from the package `circular` but without forcing the data format `circular`. It also adds functions to calculate summary statistics of cyclical data in tidy workflows.
  
## Key features
* Circular kernel density estimation in `stat_density_circular` for `geom_density_circular`.
* Add modes (peaks) and antimodes (troughs/valleys) to regular and cyclical density plots with `stat_mode`, `stat_mode_circular`, and `geom_mode_point`.
* Calculate angular mean `angular_mean()`, weighted angular mean `weighted_angular_mean()`, and angular standard deviations `angular_sd()`, also with the approximation from Yamartino 1984 `angular_sd_yamartino()`.
* Check if an angle falls within an angular range with `is_angle_in_range()`, which is useful to asign directions.

![](man/figures/better_density-2.png)
