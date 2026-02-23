# Set working directory
setwd("/Users/tybeal/Library/CloudStorage/GoogleDrive-tbeal@ucdavis.edu/My Drive/GAIN/NVS")

# Read the CSV file
d <- read.csv("Manuscript/Data/NVS_subscores_29July2024.csv")

# Install and load the plotrix package if not already installed
if (!requireNamespace("plotrix", quietly = TRUE)) {
    install.packages("plotrix")
}
library(plotrix)

# Function to create a radar plot for a single food
create_radar_plot <- function(food_data, food_name) {
    # Extract the values for the radar plot
    values <- as.numeric(food_data[2:8])
    
    # Get the original column names
    orig_labels <- names(food_data)[2:8]
    
    # Create custom labels
    labels <- orig_labels
    labels[labels == "Omega_3"] <- "Omega-3"
    labels[labels == "Nutrient_ratios"] <- "Nutrient\nratios"
    
    # Set up the radar chart
    radial.plot(
        values,
        labels = labels,
        rp.type = "p",
        main = food_name,
        radial.lim = c(0, 100),
        line.col = "#156082",
        poly.col = adjustcolor("lightblue", alpha.f = 0.5),
        show.grid.labels = FALSE  # Hide default grid labels
    )
    
    # Add custom grid labels on top of the plot
    for (i in 1:5) {
        text(0, i*20, paste0(i*20), adj = c(1.1, -0.2), cex = 0.8)
    }
}

# Create a PDF file to save all radar plots
pdf("Manuscript/Figures/radar_plots_29July2024.pdf", width = 5, height = 5)

# Loop through each food and create a radar plot
for (i in 1:nrow(d)) {
    create_radar_plot(d[i, ], d$Food[i])
}

# Close the PDF device
dev.off()