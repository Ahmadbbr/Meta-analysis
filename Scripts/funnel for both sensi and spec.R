# Load required packages
library(meta)


# Read data from Excel
data <- data.frame(
  Author = c("Chen et al 2024", "John et al 2023", "Moore et al 2023", "De vos et al 2024",
             "Liu et al 2023", "Sossen et al 2024", "Koo and yoon et al 2024", "Shi et al 2021",
             "Huerga et al 2023", "Phunphae et al 2024", "Song et al 2021", "Muyoyeta et al 2021",
             "Dippenaar et al 2021", "Li et al 2022", "Broger et al 2020", "Abdulgader et al 2024",
             "Gao et al 2024", "Sawatpanich 2022", "Fu et al 2022", "Yuan et al 2024",
             "Kagujje et al 2022", "Belotte et al 2022"),
  TP = c(194.5, 76.5, 62.5, 65.5, 604.5, 84.5, 27.5, 141.5, 68.5, 93.5, 38.5, 26.5, 
         294.5, 24.5, 59.5, 136.5, 82.5, 845.5, 406.5, 99.5, 116.5, 278.5),
  FN = c(17.5, 9.5, 9.5, 6.5, 271.5, 178.5, 7.5, 62.5, 859.5, 28.5, 8.5, 8.5,
         27.5, 9.5, 52.5, 25.5, 11.5, 215.5, 68.5, 16.5, 18.5, 16.5),
  FP = c(16.5, 348.5, 0.5, 0.5, 12.5, 12.5, 9.5, 39.5, 133.5, 2.5, 3.5, 9.5,
         17.5, 0.5, 3.5, 12.5, 1.5, 468.5, 85.5, 378.5, 423.5, 47.5),
  TN = c(773.5, 588.5, 29.5, 29.5, 396.5, 396.5, 71.5, 3899.5, 46.5, 6.5, 52.5, 108.5,
         259.5, 5.5, 258.5, 239.5, 16.5, 8067.5, 2634.5, 1634.5, 226.5, 1122.5)
)


# Calculate logit-transformed values and standard errors
data <- within(data, {
  # Sensitivity calculations
  logit_sens <- log(TP/(TP + FN))  # Logit sensitivity
  se_logit_sens <- sqrt(1/TP + 1/FN)  # Standard error
  
  # Specificity calculations
  logit_spec <- log(TN/(TN + FP))  # Logit specificity
  se_logit_spec <- sqrt(1/TN + 1/FP)  # Standard error
})

# View key columns
head(data[, c("Author", "logit_sens", "se_logit_sens", "logit_spec", "se_logit_spec")])

# Meta-analysis using inverse variance method (for funnel/egger)
meta_sens <- metagen(TE = logit_sens, seTE = se_logit_sens, studlab = Author, data = data)
meta_spec <- metagen(TE = logit_spec, seTE = se_logit_spec, studlab = Author, data = data)

# Publication bias assessment
# 1. Funnel plots
par(mfrow = c(1, 2))
funnel(meta_sens, main = "Sensitivity (Logit Scale)")
funnel(meta_spec, main = "Specificity (Logit Scale)")

egger_test <- regtest(meta_sens, predictor = "sei", model = "rma")

# 2. Egger's test
egger_sens <- metabias(meta_sens, method = "linreg", plotit = TRUE)
egger_spec <- metabias(meta_spec, method = "linreg", plotit = TRUE)

# Print results
cat("=== Sensitivity ===\n",
    "Egger's test p-value:", egger_sens$p.value, "\n\n",
    "=== Specificity ===\n",
    "Egger's test p-value:", egger_spec$p.value)