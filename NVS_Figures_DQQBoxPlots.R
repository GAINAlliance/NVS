setwd("/Users/tybeal/Library/CloudStorage/GoogleDrive-tbeal@ucdavis.edu/My Drive/GAIN/NVS")
d <- read.csv(text = readLines("Manuscript/Data/NVS_26July2024.csv"))

# Get duplicate rows based on Food column
duplicate_rows <- duplicated(d$Food_long)

# Remove duplicate rows
d <- d[!duplicate_rows,]

d <- d[, c("Food", "Food_group", "DQQ_question", "DQQ_food_group", "NVS")]

d$DQQ_question <- sub("\\...*", "", d$DQQ_question)

# Calculate summarized means
means <- with(d, tapply(NVS, DQQ_food_group, FUN = mean, na.rm=TRUE))

# Assign means back to original data frame
d$mean_NVS <- means[as.character(d$DQQ_food_group)]

# Order by medians
medians <- tapply(d$NVS, d$DQQ_food_group, median, na.rm = TRUE)
order <- order(medians)
d$DQQ_food_group <- factor(d$DQQ_food_group, levels = names(medians)[order])

# Define color palette
palette <- c("#5e5766", "#c8875e", "#6f9894", "#e6b123", "#b95547", "#b7c1bd")

# Create a named vector mapping Food_group to colors
food_group_colors <- c(
    "Fruits" = palette[1],
    "Starchy staples" = palette[2],
    "Vegetables" = palette[3],
    "Pulses, nuts, and seeds" = palette[4],
    "Animal-source foods" = palette[5],
    "Ultra-processed foods" = palette[6]
)

# Custom function to assign colors based on food group
assign_color <- function(food_group) {
    return(food_group_colors[food_group])
}

# Apply the custom function to assign colors
d$color <- sapply(d$Food_group, assign_color)

# Plot
cairo_pdf("Manuscript/Figures/Fig 2_DQQ_26July2024.pdf", width = 8, height = 8)

quartzFonts(whitney = c("Whitney Book", "Whitney Black", "Whitney Book Oblique", "Whitney Black Oblique"))
par(family = "whitney")
par(mar = c(5, 16, 1, 2))  # Adjusted margins: bottom, left, top, right

# Set up the plot area with custom borders
plot.new()
plot.window(xlim = c(0, 100), ylim = c(0.5, length(unique(d$DQQ_food_group)) + 0.5), xaxs = "i", yaxs = "i")

# Draw bottom and left axes
axis(1, at = seq(0, 100, by = 20))
axis(2, at = 1:length(unique(d$DQQ_food_group)), labels = levels(d$DQQ_food_group), las = 2, tick = TRUE)

# Add box with only bottom and left sides
box(bty = "L")

# Add boxplots with xpd = TRUE
par(xpd = TRUE)
boxplot(NVS ~ DQQ_food_group, data = d, add = TRUE, horizontal = TRUE, axes = FALSE,
        col = d$color[match(levels(d$DQQ_food_group), d$DQQ_food_group)],
        outcol = d$color[match(levels(d$DQQ_food_group), d$DQQ_food_group)],
        outpch = 20)
par(xpd = FALSE)

# Add x-axis label
mtext("Nutritional Value Score", side = 1, line = 3)

legend_order <- c("Fruits", "Vegetables", "Pulses, nuts, and seeds", "Animal-source foods", "Starchy staples", "Ultra-processed foods")

# Add legend with adjusted spacing
legend("bottomright", 
       legend = legend_order,
       fill = palette[c(1, 3, 4, 5, 2, 6)], 
       border = NA, 
       bty = "n",  # This removes the box
       box.col = "transparent", 
       xpd = TRUE, 
       cex = 0.9,
       x.intersp = 0.5)

dev.off()