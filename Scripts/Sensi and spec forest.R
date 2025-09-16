# Load required libraries
library(meta)
#install.packages("gridExtra")
library(gridExtra)

# Create dataset
data <- data.frame(
  Author = c("Chen et al 2024", "John et al 2023", "Moore et al 2023", "De vos et al 2024",
             "Liu et al 2023", "Sossen et al 2024", "Koo and Yoon et al 2024", "Shi et al 2021",
             "Huerga et al 2023", "Phunphae et al 2024", "Song et al 2021", "Muyoyeta et al 2021",
             "Dippenaar et al 2021", "Li et al 2022", "Broger et al 2020", "Abdulgader et al 2024",
             "Gao et al 2024", "Sawatpanich 2022", "Fu et al 2022", "Yuan et al 2024",
             "Kagujje et al 2022", "Belotte et al 2022"),
  Sens = c(0.917, 0.89, 0.868, 0.91, 0.69, 0.321, 0.786, 0.694, 0.074, 0.766, 0.819, 0.757,
           0.915, 0.721, 0.531, 0.843, 0.878, 0.797, 0.856, 0.858, 0.863, 0.944),
  Spec = c(0.979, 0.628, 0.983, 0.983, 0.969, 0.969, 0.883, 0.99, 0.258, 0.722, 0.938, 0.919,
           0.937, 0.917, 0.987, 0.95, 0.917, 0.945, 0.969, 0.812, 0.348, 0.959)
)

# Add confidence intervals (approximate 95% CI for demonstration)
data$Sens_lower <- pmax(0, data$Sens - 0.05)
data$Sens_upper <- pmin(1, data$Sens + 0.05)
data$Spec_lower <- pmax(0, data$Spec - 0.05)
data$Spec_upper <- pmin(1, data$Spec + 0.05)

# Meta-analysis for Sensitivity
meta_sens <- metaprop(
  event = round(data$Sens * 100),  # Convert to count (approximate)
  n = rep(100, nrow(data)),        # Placeholder sample size
  studlab = data$Author,
  sm = "PLOGIT",
  method.tau = "ML"
)

# Meta-analysis for Specificity
meta_spec <- metaprop(
  event = round(data$Spec * 100),  # Convert to count (approximate)
  n = rep(100, nrow(data)),        # Placeholder sample size
  studlab = data$Author,
  sm = "PLOGIT",
  method.tau = "ML"
)

# Create forest plots
forest_sens <- forest(meta_sens, 
                      leftcols = "studlab",
                      rightcols = c("effect", "ci"),
                      smlab = "Sensitivity",
                      xlim = c(0, 1),
                      xlab = "Proportion (95% CI)",
                      print.I2 = TRUE,
                      print.tau2 = TRUE,
                      print.p = TRUE,
                      col.square = "blue",
                      col.diamond = "darkblue",
                      col.inside = "white")

forest_spec <- forest(meta_spec, 
                      leftcols = "studlab",
                      rightcols = c("effect", "ci"),
                      smlab = "Specificity",
                      xlim = c(0, 1),
                      xlab = "Proportion (95% CI)",
                      print.I2 = TRUE,
                      print.tau2 = TRUE,
                      print.p = TRUE,
                      col.square = "red",
                      col.diamond = "darkred",
                      col.inside = "white")

# Combine plots side by side
grid.arrange(forest_sens, forest_spec, ncol = 2)

# Save to file
# png("combined_forest_plot.png", width = 14, height = 10, units = "in", res = 300)
# grid.arrange(forest_sens, forest_spec, ncol = 2)
# dev.off()