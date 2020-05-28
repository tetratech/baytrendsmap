# baytrendsmap

Shiny app to create maps on-the-fly from output of the baytrends package.

# baytrends

https://CRAN.R-project.org/package=baytrends 

The baytrends package was developed to enable users to evaluate long-term trends in the Chesapeake Bay using a Generalized Additive Modeling (GAM) approach. The model development includes selecting a GAM structure to describe nonlinear seasonally-varying changes over time, incorporation of hydrologic variability via either a river flow or salinity, the use of an intervention to deal with method or laboratory changes suspected to impact data values, and representation of left- and interval-censored data. This approach, which is fully transferable to other systems, allows for Chesapeake Bay water quality data to be evaluated in a statistically rigorous, yet flexible way to provide insights to a range of management- and research-focused questions.

# Installation

``` r
library(devtools)  #install if needed
#Sys.setenv("TAR" = "internal") # needed for use with R v3.6.0
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