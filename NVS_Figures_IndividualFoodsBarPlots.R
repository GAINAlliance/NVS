setwd("/Users/tybeal/Library/CloudStorage/GoogleDrive-tbeal@ucdavis.edu/My Drive/GAIN/NVS")
d <- read.csv(text = readLines("Manuscript/Data/NVS_26July2024.csv"))

# Get duplicate rows based on Food column
duplicate_rows <- duplicated(d$Food)

# Remove duplicate rows
d <- d[!duplicate_rows,]

d <- d[, c("Food", "Food_group", "NVS")]

d <- d[d$Food == "Dried okra" | d$Food == "Spinach"| d$Food == "Chicken organs"| d$Food == "Bivalves"| d$Food == "Fatty fish"| d$Food == "Lean fish" | d$Food == "Crustaceans"| d$Food == "Zucchini"| d$Food == "Sunflower seeds"| d$Food == "Edamame"| d$Food == "Beef"| d$Food == "Pork"| d$Food == "Eggs"| d$Food == "Carrots"| d$Food == "Lentils"| d$Food == "Chicken"| d$Food == "Tofu"| d$Food == "Almonds"| d$Food == "Whole cow milk"| d$Food == "Orange fleshed sweet potato"| d$Food == "Guava"| d$Food == "Unsweetened soymilk"| d$Food == "Potato"| d$Food == "Mango"| d$Food == "Corn"| d$Food == "Whole wheat pasta"| d$Food == "Brown rice" | d$Food == "Refined wheat pasta" | d$Food == "Watermelon"| d$Food == "White rice" | d$Food == "Congee (rice porridge)" | d$Food == "Skimmed milk" | d$Food == "Dried small fish" | d$Food == "Cheerios (fortified)" | d$Food == "Lucky Charms (fortified)" | d$Food == "Sweet potato chips" | d$Food == "Hot dog" | d$Food == "Gatorade" | d$Food == "Saltine crackers (fortified)" | d$Food == "Sugar cookies (fortified)" | d$Food == "Sardines" | d$Food == "Coconut" | d$Food == "Fufu",]

order <- c("Fruits", "Vegetables", "Pulses, nuts, and seeds",  
           "Animal-source foods", "Starchy staples", "Ultra-processed foods")

d <- d[order(d$NVS),]

cairo_pdf("Manuscript/Figures/Fig 3_Single foods_26July2024.pdf", width = 6, height = 10)
quartzFonts(whitney = c("Whitney Book", "Whitney Black", "Whitney Book Oblique", "Whitney Black Oblique"))
par(family = "whitney")
par(oma = c(3, 7.5, 0, 1))  # Increased bottom margin for x-axis label
palette <- c("#5e5766", "#c8875e", "#6f9894", "#e6b123", "#b95547", "#b7c1bd")

food_group_idx <- c(
    "Fruits" = 1,
    "Starchy staples" = 2, 
    "Vegetables" = 3,
    "Pulses, nuts, and seeds" = 4,
    "Animal-source foods" = 5,
    "Ultra-processed foods" = 6
)

# Map Food_group to index
d$food_group_idx <- food_group_idx[d$Food_group]

barplot(d$NVS, names.arg = d$Food, horiz = T, las = 1, col = palette[d$food_group_idx], border = NA, space = .1, xlim = c(0, 100), xlab = "Nutritional Value Score")

abline(v = 0, lty = 1, lwd = 2)

# Get bar spacing  
spacing <- barplot(d$NVS, space = 0.1, plot = FALSE)

# Calculate cumulative spacing
positions <- cumsum(spacing)

# Add labels
text(x = d$NVS, 
     y = spacing - .1,
     labels = round(d$NVS, 0),
     pos = 4,
     xpd = TRUE,
     font = 2,
     col = palette[d$food_group_idx])

legend(45,  7,
       legend = order,
       fill = palette[c(1, 3, 4, 5, 2, 6)], border = NA, box.col = "transparent", xpd = T, cex = .9, x.intersp = 0.5)

dev.off()