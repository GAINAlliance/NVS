# Nutritional Value Score (NVS)

Code for calculating the Nutritional Value Score (NVS), a food rating system that integrates nutrient density with indicators of noncommunicable disease prevention.

## Citation

Beal, T. Ortenzi, F. (2026). Nutritional value score rates foods by nutrient density and noncommunicable disease prevention. *Journal of Nutrition*. DOI: [10.1016/j.tjnut.2026.101443](https://doi.org/10.1016/j.tjnut.2026.101443)

## Overview

The NVS rates individual foods on a 1–100 scale by combining seven sub-scores into a weighted composite:

**NVS = (2·V + 2·M + 1.25·P + 1·N3 + 0.75·F + 0.75·EMR + 2.25·NR) / 10**

where:

- **V** — Vitamin density (11 vitamins, bioavailability-adjusted)
- **M** — Mineral density (5 minerals, bioavailability-adjusted for iron and zinc)
- **P** — Protein quantity and quality (incorporating DIAAS)
- **N3** — Omega-3 fatty acids (long-chain and ALA)
- **F** — Fiber content
- **EMR** — Energy density (penalizes calorie-dense foods)
- **NR** — Nutrient ratios (saturated-to-unsaturated fat, carbohydrate-to-fiber, sodium-to-potassium)

Each sub-score uses a dual reference unit approach, combining a per-energy basis (per 300 kcal) with a per-mass basis (per 100 g, scaled to 231 g), except for liquids which use the energy basis only. Ultra-processed foods receive a 25% penalty. Foods fortified with 4+ vitamins or 2+ minerals are also penalized.

## Repository Structure

```
├── LICENSE
├── README.md
├── FCD_NVS_26July2024.csv                  # Input: food composition data (414 foods, 47 variables)
├── RNI.csv                                 # Input: Recommended Nutrient Intake reference values
├── NVS_OutputData.xlsx                     # Output: NVS scores and sub-scores (414 foods)
├── NVS_Analysis.R                          # Main NVS calculation script
├── NVS_Figures_IndividualFoodsBarPlots.R   # Bar plots of NVS for selected foods (Fig. 3)
├── NVS_Figures_DQQBoxPlots.R               # Box plots by Diet Quality Questionnaire food groups (Fig. 2)
└── NVS_Figures_RadarPlots.R                # Radar plots of sub-scores for individual foods
```

## Data

**FCD_NVS_26July2024.csv** is the input food composition dataset containing 414 foods with 47 variables, including nutrient composition per 100 g (energy, macronutrients, 11 vitamins, 5 minerals, omega-3 fatty acids, phytate), protein quality (DIAAS), mineral bioavailability values (iron and zinc absorption rates), and food property flags (liquid, unsweetened dairy, dried/rehydrated, UPF, fortification status).

**RNI.csv** contains Recommended Nutrient Intake reference values used as denominators in the sub-score calculations, including 11 vitamin RNIs, bioavailability-specific thresholds for iron (at 10%, 15%, and 20% absorption) and zinc (at refined, semi-refined, semi-unrefined, and unrefined diet absorption levels), and RNIs for calcium, potassium, and magnesium.

**NVS_OutputData.xlsx** contains the calculated NVS scores and sub-scores for all 414 foods, along with food identifiers and classifications (food name, food group, DQQ food group and question).

## Scripts

### NVS_Analysis.R

The main analysis script that calculates the NVS and all sub-scores. It reads food composition data and recommended nutrient intake (RNI) values, then sequentially computes:

1. Scaling of nutrient values for dried foods consumed rehydrated
2. Vitamin sub-score (V) from 11 vitamins relative to RNI
3. Mineral sub-score (M) from 5 minerals with bioavailability adjustments for iron and zinc
4. Omega-3 sub-score (N3) from DHA, EPA, DPA, and ALA
5. Protein sub-score (P) combining quantity and DIAAS quality
6. Fiber sub-score (F)
7. Nutrient ratios (NR): saturated-to-unsaturated fat ratio (SUR), carbohydrate-to-fiber ratio (CFR), and sodium-to-potassium ratio (NaKR)
8. Energy density (EMR)
9. Final NVS composite and nutritional Functional Unit (nFU) calculations

### NVS_Figures_IndividualFoodsBarPlots.R

Generates horizontal bar plots showing NVS scores for a curated set of individual foods across six food groups (fruits, vegetables, pulses/nuts/seeds, animal-source foods, starchy staples, and ultra-processed foods).

### NVS_Figures_DQQBoxPlots.R

Generates box plots showing the distribution of NVS scores across Diet Quality Questionnaire (DQQ) food group categories, ordered by median score.

### NVS_Figures_RadarPlots.R

Generates radar plots displaying the seven sub-score components for individual foods, using the `plotrix` package.

## Dependencies

- R (base R; no external packages required for the main analysis)
- `plotrix` (for radar plots only)

## License

This code is released under a non-commercial open access license. See [LICENSE](LICENSE) for details. Commercial use requires a separate license from the Global Alliance for Improved Nutrition (GAIN). Contact tbeal@gainhealth.org for inquiries.