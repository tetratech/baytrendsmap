# Create "blank" plot for dynamic map plot link
# Erik.Leppo@tetratech.com
# 2021-09-08
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2023-10-12, update to include DIN and PO4 for surface only


# Packages
library(ggplot2)

# Plot
p_blank <- ggplot() + 
  theme_void() +
  labs(title = "No trends plot available for selected dataset."
       , subtitle = "Trends plots only available for 'Long Term' dataset.
  Parameters DIN and PO4 are only available for surface layer.")

# Save
ggsave("_no_plot.png", bg = "white")