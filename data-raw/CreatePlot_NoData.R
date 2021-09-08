# Create "blank" plot for dynamic map plot link
# Erik.Leppo@tetratech.com
# 2021-09-08
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Packages
library(ggplot2)

# Plot
p_blank <- ggplot() + 
  theme_void() +
  labs(title = "No trends plot available for selected dataset."
       , subtitle = "Trends plots only available for 'Full Period' dataset.")

# Save
ggsave("_no_plot.png")