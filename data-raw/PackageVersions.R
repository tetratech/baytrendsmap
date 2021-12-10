# Install old version of package
# Erik.Leppo@tetratech.com
# 2021-12-10
#~~~~~~~~~~~~~~~~~


# shiny
devtools::install_version("shiny"
                          , version = "1.5.0"
                          , repos = "http://cran.us.r-project.org")

# warning about bslib::get_current_theme() requires shiny v1.6.0 or greater
# don't use this function or package