# Define the custom theme

source("https://raw.githubusercontent.com/jg44/JGTools/master/custom_themeJG.R")

print(custom_themeJG)
.sa()

custom_themeJG <- theme_classic() + 
    theme(
        text = element_text(size = 24),          # Global text size
        axis.title = element_text(size = 24),    # Axis labels
        axis.text = element_text(size = 20),     # Axis tick labels
        legend.text = element_text(size = 20),   # Legend text
        legend.title = element_text(size = 22),  # Legend title
        axis.title.y = element_text(margin = margin(r = 10)),  # Increase y-axis label distance
        axis.title.x = element_text(margin = margin(t = 10)),  # Move x-axis label down
        axis.text.x = element_text(margin = margin(t = 10)),   # Move x-axis tick labels down
        legend.position = c(0.85, 0.85),         # Position inside, top-right corner
        legend.justification = c(1, 1),          # Anchor legend to bottom-right
        legend.background = element_rect(fill = "white", color = NA),  # White background for legend
        legend.box.just = "right",  # Align legend box to the right
        legend.box.margin = margin(5, 5, 5, 5),  # Inset the legend box slightly
        plot.margin = margin(b = 10, r = 10, l = 15)  # Set bottom, right, and left margins (left margin increased to 15)
    )
