# baytrendsmap

Shiny app to create maps on-the-fly from output of the baytrends package.

# baytrends

https://CRAN.R-project.org/package=baytrends 

The baytrends package was developed to enable users to evaluate long-term trends in the Chesapeake Bay using a Generalized Additive Modeling (GAM) approach. The model development includes selecting a GAM structure to describe nonlinear seasonally-varying changes over time, incorporation of hydrologic variability via either a river flow or salinity, the use of an intervention to deal with method or laboratory changes suspected to impact data values, and representation of left- and interval-censored data. This approach, which is fully transferable to other systems, allows for Chesapeake Bay water quality data to be evaluated in a statistically rigorous, yet flexible way to provide insights to a range of management- and research-focused questions.

# Badges
<!-- badges: start -->
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/tetratech/baytrendsmap/graphs/commit-activity)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://www.tidyverse.org/lifecycle/#stable)

[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)

[![R-CMD-check](https://github.com/leppott/baytrends_map/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/leppott/baytrends_map/actions/workflows/R-CMD-check.yaml)

[![GitHub issues](https://img.shields.io/github/issues/tetratech/baytrendsmap.svg)](https://GitHub.com/tetratech/baytrendsmap/issues/)
<!-- badges: end -->

# Installation

``` r
if(!require(remotes)){install.packages("remotes")}  #install if needed
install_github("tetratech/baytrendsmap", force=TRUE, build_vignettes=TRUE, dependencies = TRUE)
```

# Usage
No functions other than running the Shiny app.

```r
library(baytrendsmap)
runShiny()
```

# Purpose

To aid staff in creating maps and exploring output of baytrends package.
