setwd("/Users/tybeal/Library/CloudStorage/GoogleDrive-tbeal@ucdavis.edu/My Drive/GAIN/NVS")
d <- read.csv("Manuscript/Data/FCD_NVS_26July2024.csv")

RNI <- read.csv("Data/RNI.csv")
# Reshape RNI to column vector
d2 <- setNames(as.vector(t(RNI)), names(RNI))

# Function to scale nutrient values for dried foods consumed rehydrated
scale_nutrients <- function(d, nutrients) {
    # Filter for dried foods consumed rehydrated
    dried_foods <- d[d$Dried_consumed_rehydrated == "Yes", ]
    
    if (nrow(dried_foods) > 0) {
        for (i in 1:nrow(dried_foods)) {
            food <- dried_foods[i, ]
            scale_factor <- food$Energy_kcal / food$Energy_kcal_fresh
            
            # Scale all specified nutrients
            d[d$Food == food$Food, nutrients] <- 
                d[d$Food == food$Food, nutrients] / scale_factor
        }
    }
    
    return(d)
}

# List of nutrients to scale
nutrients_to_scale <- c("Energy_kcal", "Carbohydrates_g", "Fiber_g", "Protein_g", 
                        "Monounsaturated_FAs_g", "Polyunsaturated_FAs_g", 
                        "Unsaturated_FAs_g", "Saturated_FAs_g", "VitA_RAE_mcg", 
                        "VitC_mg", "VitD_mcg", "VitE_mg", "Thiamin_mg", 
                        "Riboflavin_mg", "Niacin_mg", "VitB6_mg", "Folate_DFE_mcg", 
                        "VitB12_mcg", "Choline_mg", "Iron_mg", "Zinc_mg", 
                        "Calcium_mg", "Potassium_mg", "Magnesium_mg", "Sodium_mg", 
                        "Phytate_mg", "DHA_g", "EPA_g", "DPA_g", "ALA_g")

# Apply the scaling function to your dataframe and overwrite the original
d <- scale_nutrients(d, nutrients_to_scale)

#######################################################################
# Calculate V_E

# Extract vitamins and energy 
v_E <- d[, c("Energy_kcal", "VitA_RAE_mcg", "VitC_mg", "VitD_mcg", "VitE_mg", "Thiamin_mg", "Riboflavin_mg", "Niacin_mg", "VitB6_mg", "Folate_DFE_mcg", "VitB12_mcg", "Choline_mg")]

# Adjust to per 300 kcal
v_E[, -1] <- sweep(v_E[, -1], 1, 300/v_E$Energy_kcal, FUN="*")

# Drop energy
v_E <- v_E[, -1]

# Calculate proportion of RNI
prop_RNI <- sweep(v_E, 2, d2[c("VitA_mcg_RAE", "VitC_mg", "VitD_mcg", "VitE_mg", "Thiamin_mg", "Ribflavin_mg", "Niacin_mg", "VitB6_mg", "Folate_mcg_DFE", "VitB12_mcg", "Choline_mg")], FUN="/")

# Cap proportions at 1  
prop_RNI[prop_RNI > 1] <- 1

# Calculate average capped proportion
d$V_E <- 100*rowMeans(prop_RNI, na.rm=TRUE)

# Normalize to 1-100 scale 
d$V_E <- (d$V_E - min(d$V_E)) * (100 - 1) / (max(d$V_E) - min(d$V_E)) + 1

# Clip normalized values to 1-100
d$V_E <- pmin(100, pmax(1, d$V_E))


# Calculate V_M

# Extract vitamins 
v_M <- d[, c("VitA_RAE_mcg", "VitC_mg", "VitD_mcg", "VitE_mg", "Thiamin_mg", "Riboflavin_mg", "Niacin_mg", "VitB6_mg", "Folate_DFE_mcg", "VitB12_mcg", "Choline_mg")]

# Adjust to per 231 g
v_M <- sweep(v_M, 1, 2.31, FUN="*")

# Calculate proportion of RNI
prop_RNI <- sweep(v_M, 2, d2[c("VitA_mcg_RAE", "VitC_mg", "VitD_mcg", "VitE_mg", "Thiamin_mg", "Ribflavin_mg", "Niacin_mg", "VitB6_mg", "Folate_mcg_DFE", "VitB12_mcg", "Choline_mg")], FUN="/")

# Cap proportions at 1  
prop_RNI[prop_RNI > 1] <- 1

# Calculate average capped proportion
d$V_M <- 100*rowMeans(prop_RNI, na.rm=TRUE)

# Normalize to 1-100 scale 
d$V_M <- (d$V_M - min(d$V_M)) * (100 - 1) / (max(d$V_M) - min(d$V_M)) + 1

# Clip normalized values to 1-100
d$V_M <- pmin(100, pmax(1, d$V_M))

d$V <- d$V_E + d$V_M

# Normalize to 1-100 scale 
d$V <- (d$V - min(d$V)) * (100 - 1) / (max(d$V) - min(d$V)) + 1

# Clip normalized values to 1-100
d$V <- pmin(100, pmax(1, d$V))

# For liquids and semi-liquid dairy (and alternatives), use energy only as the reference unit.
d$V <- ifelse(d$Liquids == "Yes", d$V_E, d$V)

# Penalize foods fortified with 4 or more vitamins (let most fortified staple foods, milks, and alternative milks not be penalized. Penalize fortified breakfast cereals and energy drinks)
d$V <- pmin(100, pmax(1, d$V*ifelse(d$Fortified_4Vitamins == "Yes", 0.75, 1)))

#######################################################################
# Calculate M_E

# Extract minerals and energy 
m_E <- d[, c("Energy_kcal", "Iron_mg", "Zinc_mg", "Calcium_mg", "Potassium_mg", "Magnesium_mg", "Iron_Abs", "Zinc_Abs")]

# Adjust to per 300 kcal
m_E[, c("Iron_mg", "Zinc_mg", "Calcium_mg", "Potassium_mg", "Magnesium_mg")] <- 
    sweep(m_E[, c("Iron_mg", "Zinc_mg", "Calcium_mg", "Potassium_mg", "Magnesium_mg")], 1, 300/m_E$Energy_kcal, FUN="*")

# Drop energy
m_E <- m_E[, -1]

# Calculate proportion of RNI for Ca, K, and Mg
prop_RNI <- sweep(m_E[, c("Calcium_mg", "Potassium_mg", "Magnesium_mg")], 2, d2[c("Calcium_mg", "Potassium_mg", "Magnesium_mg")], FUN="/")

# Calculate proportion fo RNI for Iron, adjusting for bioavailability
prop_RNI$Iron_mg <- m_E$Iron_mg/ifelse(m_E$Iron_Abs == 0.2, d2["Iron_mg_20"], 
                                       ifelse(m_E$Iron_Abs == 0.15, d2["Iron_mg_15"], 
                                              d2["Iron_mg_10"]))

# Calculate proportion fo RNI for Zinc, adjusting for bioavailability
prop_RNI$Zinc_mg <- m_E$Zinc_mg/ifelse(m_E$Zinc_Abs == 0.44, d2["Zinc_mg_R"], 
                                       ifelse(m_E$Zinc_Abs == 0.35, d2["Zinc_mg_SR"],
                                              ifelse(m_E$Zinc_Abs == 0.3, d2["Zinc_mg_SU"], 
                                                     d2["Zinc_mg_U"])))

# Cap proportions at 1  
prop_RNI[prop_RNI > 1] <- 1

# Calculate average capped proportion
d$M_E <- 100*rowMeans(prop_RNI, na.rm=TRUE)

# Normalize to 1-100 scale 
d$M_E <- (d$M_E - min(d$M_E)) * (100 - 1) / (max(d$M_E) - min(d$M_E)) + 1

# Clip normalized values to 1-100
d$M_E <- pmin(100, pmax(1, d$M_E))


# Calculate M_M
# Extract minerals 
m_M <- d[, c("Iron_mg", "Zinc_mg", "Calcium_mg", "Potassium_mg", "Magnesium_mg", "Iron_Abs", "Zinc_Abs")]

# Adjust to per 231 g
m_M <- sweep(m_M, 1, 2.31, FUN="*")

# Calculate proportion of RNI for Ca, K, and Mg
prop_RNI <- data.frame(
    Calcium_mg = m_M$Calcium_mg / d2["Calcium_mg"],
    Potassium_mg = m_M$Potassium_mg / d2["Potassium_mg"],
    Magnesium_mg = m_M$Magnesium_mg / d2["Magnesium_mg"]
)

# Calculate proportion of RNI for Iron, adjusting for bioavailability
prop_RNI$Iron_mg <- m_M$Iron_mg / ifelse(m_M$Iron_Abs == 0.2, d2["Iron_mg_20"], 
                                         ifelse(m_M$Iron_Abs == 0.15, d2["Iron_mg_15"], 
                                                d2["Iron_mg_10"]))

# Calculate proportion of RNI for Zinc, adjusting for bioavailability
prop_RNI$Zinc_mg <- m_M$Zinc_mg / ifelse(m_M$Zinc_Abs == 0.44, d2["Zinc_mg_R"], 
                                         ifelse(m_M$Zinc_Abs == 0.35, d2["Zinc_mg_SR"],
                                                ifelse(m_M$Zinc_Abs == 0.3, d2["Zinc_mg_SU"], 
                                                       d2["Zinc_mg_U"])))

# Cap proportions at 1  
prop_RNI[prop_RNI > 1] <- 1

# Calculate average capped proportion
d$M_M <- 100 * rowMeans(prop_RNI, na.rm=TRUE)

# Normalize to 1-100 scale 
d$M_M <- (d$M_M - min(d$M_M)) * (100 - 1) / (max(d$M_M) - min(d$M_M)) + 1

# Clip normalized values to 1-100
d$M_M <- pmin(100, pmax(1, d$M_M))

d$M <- d$M_E + d$M_M

# Normalize to 1-100 scale 
d$M <- (d$M - min(d$M)) * (100 - 1) / (max(d$M) - min(d$M)) + 1

# Clip normalized values to 1-100
d$M <- pmin(100, pmax(1, d$M))

# For liquids and semi-liquid dairy (and alternatives), use energy only as the reference unit.
d$M <- ifelse(d$Liquids == "Yes", d$M_E, d$M)

# Penalize foods fortified with 2 or more minerals (let most fortified staple foods, milks, and alternative milks not be penalized. Penalize fortified breakfast cereals and energy drinks)
d$M <- pmin(100, pmax(1, d$M*ifelse(d$Fortified_2Minerals == "Yes", 0.75, 1)))

#######################################################################
# Calculate N3_E

# Extract N3 and energy 
n3_E <- d[, c("Energy_kcal", "DHA_g", "EPA_g", "DPA_g", "ALA_g")]

# Adjust to per 300 kcal
n3_E[, c("DHA_g", "EPA_g", "DPA_g", "ALA_g")] <- 
    sweep(n3_E[, c("DHA_g", "EPA_g", "DPA_g", "ALA_g")], 1, 300/n3_E$Energy_kcal, FUN="*")

n3_E$LCN3 <- n3_E$DHA_g + n3_E$EPA_g + n3_E$DPA_g

d$N3_E <- pmax(n3_E$LCN3/0.25, n3_E$ALA_g/1.24)

# Normalize to 1-100 scale 
d$N3_E <- (d$N3_E - min(d$N3_E)) * (100 - 1) / (max(d$N3_E) - min(d$N3_E)) + 1

# Clip normalized values to 1-100
d$N3_E <- pmin(100, pmax(1, d$N3_E))

# Calculate N3_M

# Extract N3
n3_M <- d[, c("DHA_g", "EPA_g", "DPA_g", "ALA_g")]

# Adjust to per 231 g
n3_M <- sweep(n3_M, 1, 2.31, FUN="*")

n3_M$LCN3 <- n3_M$DHA_g + n3_M$EPA_g + n3_M$DPA_g

d$N3_M <- pmax(n3_M$LCN3/0.25, n3_M$ALA_g/1.24)

# Normalize to 1-100 scale 
d$N3_M <- (d$N3_M - min(d$N3_M)) * (100 - 1) / (max(d$N3_M) - min(d$N3_M)) + 1

# Clip normalized values to 1-100
d$N3_M <- pmin(100, pmax(1, d$N3_M))

d$N3 <- d$N3_E + d$N3_M

# Normalize to 1-100 scale 
d$N3 <- (d$N3 - min(d$N3)) * (100 - 1) / (max(d$N3) - min(d$N3)) + 1

# Clip normalized values to 1-100
d$N3 <- pmin(100, pmax(1, d$N3))

# For liquids and semi-liquid dairy (and alternatives), use energy only as the reference unit.
d$N3 <- ifelse(d$Liquids == "Yes", d$N3_E, d$N3)

#######################################################################
# Calculate Protein_E

# Extract EAAs, DIASS, and energy 
Protein_E <- d[, c("Energy_kcal", "Protein_g", "DIAAS")]

# Adjust to per 300 kcal
Protein_E$Protein_g <- Protein_E$Protein_g*300/Protein_E$Energy_kcal

d$Protein_E <- Protein_E$Protein_g

# Normalize to 1-100 scale 
d$Protein_E <- (d$Protein_E - min(d$Protein_E)) * (100 - 1) / (max(d$Protein_E) - min(d$Protein_E)) + 1

# Clip normalized values to 1-100
d$Protein_E <- pmin(100, pmax(1, d$Protein_E))

# Calculate Protein_M

# Extract Protein and DIASS
Protein_M <- d[, c("Protein_g", "DIAAS")]

# Adjust to per 231 g
Protein_M$Protein_g <- Protein_M$Protein_g*2.31

d$Protein_M <- Protein_M$Protein_g

# Normalize to 1-100 scale 
d$Protein_M <- (d$Protein_M - min(d$Protein_M)) * (100 - 1) / (max(d$Protein_M) - min(d$Protein_M)) + 1

# Clip normalized values to 1-100
d$Protein_M <- pmin(100, pmax(1, d$Protein_M))

d$protein <- d$Protein_E + d$Protein_M

# Normalize to 1-100 scale 
d$protein <- (d$protein - min(d$protein)) * (100 - 1) / (max(d$protein) - min(d$protein)) + 1

# Clip normalized values to 1-100
d$protein <- pmin(100, pmax(1, d$protein))

# For liquids and semi-liquid dairy (and alternatives), use energy only as the reference unit.
d$protein <- ifelse(d$Liquids == "Yes", d$Protein_E, d$protein)

# Normalize to 1-100 scale 
d$DIAAS <- (d$DIAAS - min(d$DIAAS)) * (100 - 1) / (max(d$DIAAS) - min(d$DIAAS)) + 1

# Clip normalized values to 1-100
d$DIAAS <- pmin(100, pmax(1, d$DIAAS))

d$P <- d$protein + d$DIAAS

# Normalize to 1-100 scale 
d$P <- (d$P - min(d$P)) * (100 - 1) / (max(d$P) - min(d$P)) + 1

# Clip normalized values to 1-100
d$P <- pmin(100, pmax(1, d$P))

#######################################################################
# Calculate F_E

# Extract energy and Fiber_g 
f_E <- d[, c("Energy_kcal", "Fiber_g")]

# Adjust to per 300 kcal
f_E$Fiber_g <- f_E$Fiber_g*300/f_E$Energy_kcal

d$F_E <- f_E$Fiber_g

# Normalize to 1-100 scale 
d$F_E <- (d$F_E - min(d$F_E)) * (100 - 1) / (max(d$F_E) - min(d$F_E)) + 1

# Clip normalized values to 1-100
d$F_E <- pmin(100, pmax(1, d$F_E))

# Calculate F_M

# Extract Fiber_g
f_M <- d[, "Fiber_g"]

# Adjust to per 231 g
f_M <- f_M * 2.31

d$F_M <- f_M

# Normalize to 1-100 scale 
d$F_M <- (d$F_M - min(d$F_M)) * (100 - 1) / (max(d$F_M) - min(d$F_M)) + 1

# Clip normalized values to 1-100
d$F_M <- pmin(100, pmax(1, d$F_M))

d$F <- d$F_E + d$F_M

# Normalize to 1-100 scale 
d$F <- (d$F - min(d$F)) * (100 - 1) / (max(d$F) - min(d$F)) + 1

# Clip normalized values to 1-100
d$F <- pmin(100, pmax(1, d$F))

# For liquids and semi-liquid dairy (and alternatives), use energy only as the reference unit.
d$F <- ifelse(d$Liquids == "Yes", d$F_E, d$F)

#######################################################################
# Calculate NR

# Function to replace infinite values with the worst non-infinite value
replace_inf_with_worst_finite <- function(x) {
    worst_finite <- max(x[is.finite(x) & !is.na(x)], na.rm = TRUE)
    ifelse(is.infinite(x), worst_finite, x)
}

# SUR (Saturated-to-Unsaturated Ratio) calculation

# Assign NA if fat provides less than 10% of energy, otherwise calculate the ratio
d$SUR <- ifelse(9*(d$Saturated_FAs_g + d$Unsaturated_FAs_g) < d$Energy_kcal/10, NA, d$Saturated_FAs_g/d$Unsaturated_FAs_g)

# Replace infinite values with the worst non-infinite value
d$SUR <- replace_inf_with_worst_finite(d$SUR)

# Apply log transformation (adding 1 to avoid log(0))
d$SUR_log <- log(d$SUR + 1)

# Get min and max log values
SURmin_log <- min(d$SUR_log, na.rm = TRUE)
SURmax_log <- max(d$SUR_log, na.rm = TRUE)

# Normalize to -100 to 0 scale on log scale
d$SUR <- -(d$SUR_log - SURmin_log) / (SURmax_log - SURmin_log) * 100

# Assign 0 (best score) to foods that didn't qualify for ratio calculation (NA values)
d$SUR <- ifelse(is.na(d$SUR), 0, d$SUR)


# CFR (Carbohydrate-to-Fiber Ratio) calculation

# Calculate percentage of calories from protein
d$protein_pct <- 4*d$Protein_g / d$Energy_kcal * 100

# Assign NA if carbs provide less than 10% of energy or protein provides 20% or more of energy, otherwise calculate the ratio
d$CFR <- ifelse(4*d$Carbohydrates_g/d$Energy_kcal < .10 | d$protein_pct >= 20, NA, d$Carbohydrates_g/d$Fiber_g)

# Replace infinite values with the worst non-infinite value
d$CFR <- replace_inf_with_worst_finite(d$CFR)

# Assign 0 (best score) to unsweetened dairy products
d$CFR <- ifelse(d$Unsweetened_dairy == "Yes", 0, d$CFR)

# Get min and max CFR values
CFRmin <- min(d$CFR, na.rm = TRUE)
CFRmax <- max(d$CFR, na.rm = TRUE)

# Normalize to -100 to 0 scale
d$CFR <- -(d$CFR - CFRmin) / (CFRmax - CFRmin) * 100

# Assign 0 (best score) to foods that didn't qualify for ratio calculation (NA values)
d$CFR <- ifelse(is.na(d$CFR), 0, d$CFR)


# NaKR (Sodium-to-Potassium Ratio) calculation

# Assign NA if sodium-to-calorie ratio is less than 0.9 mg/kcal, otherwise calculate the ratio
d$NaKR <- ifelse(d$Sodium_mg/d$Energy_kcal < 0.9, NA, d$Sodium_mg/d$Potassium_mg)

# Replace infinite values with the worst non-infinite value
d$NaKR <- replace_inf_with_worst_finite(d$NaKR)

# Apply log transformation (adding 1 to avoid log(0))
d$NaKR_log <- log(d$NaKR + 1)

# Get min and max log values
NaKRmin_log <- min(d$NaKR_log, na.rm = TRUE)
NaKRmax_log <- max(d$NaKR_log, na.rm = TRUE)

# Normalize to -100 to 0 scale on log scale
d$NaKR <- -(d$NaKR_log - NaKRmin_log) / (NaKRmax_log - NaKRmin_log) * 100

# Assign 0 (best score) to foods that didn't qualify for ratio calculation (NA values)
d$NaKR <- ifelse(is.na(d$NaKR), 0, d$NaKR)


# Calculate composite nutrient ratio score
d$NR <- d$SUR + d$CFR + d$NaKR

# Normalize to 1-100 scale 
NRmin <- min(d$NR, na.rm = TRUE)
NRmax <- max(d$NR, na.rm = TRUE)
d$NR <- (d$NR - NRmin) * (100 - 1) / (NRmax - NRmin) + 1

# Clip normalized values to 1-100
d$NR <- pmin(100, pmax(1, d$NR))

#######################################################################
# Calculate Energy Density (EMR)
# EMR
d$EMR <- ifelse(d$Energy_kcal < 130, NA, d$Energy_kcal/100)

# Get min and max MER values
EMRmin <- min(d$EMR, na.rm = TRUE)
EMRmax <- max(d$EMR, na.rm = TRUE)

# Normalize to -100 to 0 scale
d$EMR <- -(d$EMR - EMRmin) / (EMRmax - EMRmin) * 100
d$EMR <- ifelse(is.na(d$EMR), 0, d$EMR)

############################################################
# Calculate nutrient density score
d$NDS <- (3.5*d$V + 3.5*d$M + 2*d$P + 1*d$N3)/10

# Normalize to 1-100 scale 
d$NDS <- (d$NDS - min(d$NDS)) * (100 - 1) / (max(d$NDS) - min(d$NDS)) + 1

# Clip normalized values to 1-100
d$NDS <- pmin(100, pmax(1, d$NDS))

############################################################
# Calculate nutritional value score
d$NVS <- (2*d$V + 2*d$M + 1.25*d$P + 1*d$N3 + 0.75*d$F + 0.75*d$EMR + 2.25*d$NR)/10

# Penalize UPFs by 25% while keeping the score between 1 and 100
d$NVS <- pmin(100, pmax(1, d$NVS*ifelse(d$UPF == "Yes", 0.75, 1)))

NVS <- d[, c("Food", "NVS")]

# Normalize to 1-100 scale 
d$NVS <- (d$NVS - min(d$NVS)) * (100 - 1) / (max(d$NVS) - min(d$NVS)) + 1

# Clip normalized values to 1-100
d$NVS <- pmin(100, pmax(1, d$NVS))

# Calculate mass quantity per NVS of 100
d$NVS_100 <- 231*100/d$NVS

# Calculate Calorie quantity per NVS of 100
d$NVS_100_Cal <- 300*100/d$NVS

d$Cal_1000 <- 100*1000/d$Energy_kcal

write.csv(d, "Manuscript/Data/NVS_26July2024.csv", row.names = FALSE)

############################################################
# Calculate nFUs
d <- d[, c("Food", "Food_group", "DQQ_question", "Energy_kcal", "NVS")]

# Calculate mass quantity per NVS of 100
d$nFU <- 231*100/d$NVS

# Calculate calorie quantity per NVS of 100
d$NVS_100_Cal <- 300*100/d$NVS

d$Cal_1000 <- 100*1000/d$Energy_kcal

write.csv(d, "Manuscript/Data/NVS_nFU_26July2024.csv", row.names = FALSE)