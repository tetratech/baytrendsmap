# Prepare data for use in the package
#
# Erik.Leppo@tetratech.com
# 2019-10-29
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create Shiny App help HTML and save to Shiny app www directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)
library(rmarkdown)

# 1. Convert to HTML (render) #####
# 2. Save to Shiny App www directory ####
fn_rmd <- file.path(".", "data-raw", "ShinyHelp.RMD")
dn_output <- file.path(".", "inst", "shiny-examples", "baytrendsmap", "www")
rmarkdown::render(fn_rmd, output_file = "ShinyHelp.html", output_dir = dn_output)




