# data.R
# This file defines the expected data format for the Adolescent Idiopathic Scoliosis (AIS) CT data.
# The data will be structured for a Bayesian linear regression model analyzing pedicle width differences at T1-T6 levels.

# --- Data Structure Description ---
# The data should ideally be in a long format, where each row represents a measurement for a specific vertebral level
# for a specific patient.

# Example R data.frame structure:
# patient_id: Unique identifier for each patient (e.g., P001, P002, ...)
# vertebra_level: The specific thoracic vertebral level (T1, T2, T3, T4, T5, T6). Can be represented as a factor or integer (1-6).
# pedicle_width_left_mm: Measured pedicle width on the left side in millimeters.
# pedicle_width_right_mm: Measured pedicle width on the right side in millimeters.
# pt_curve_concave_side: Indicates which side is concave for the Proximal Thoracic (PT) curve for this patient (e.g., "Right").
#                        Based on the user's description, the PT curve is typically concave on the right side.

# --- Derived Variables (for modeling) ---
# From the above, the following variables can be derived in R before passing to Stan:
# pedicle_width_concave_mm: Pedicle width on the concave side of the PT curve.
# Histogram of pedicle width difference distribution with Bayesian interpretation
library(ggplot2)

# Create histogram with Bayesian perspective
ggplot(ais_data, aes(x = pedicle_width_difference_mm)) +
    geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black", alpha = 0.7) +
    geom_vline(aes(xintercept = mean(pedicle_width_difference_mm, na.rm = TRUE)), color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = median(pedicle_width_difference_mm, na.rm = TRUE)), color = "green", linetype = "dashed", linewidth = 1) +
    labs(
        title = "Distribution of Pedicle Width Difference in AIS Patients",
        subtitle = "Red: Mean (Prior specification) | Green: Median (Robust estimate)",
        x = "Pedicle Width Difference (mm)",
        y = "Frequency",
        caption = "Bayesian interpretation: This empirical distribution informs prior specification"
    ) +
    theme_minimal() +
    theme(plot.subtitle = element_text(size = 10, color = "gray40"))

# Summary statistics for Bayesian prior elicitation
cat("Summary statistics for Bayesian prior specification:\n")
cat("Mean:", mean(ais_data$pedicle_width_difference_mm, na.rm = TRUE), "\n")
cat("SD:", sd(ais_data$pedicle_width_difference_mm, na.rm = TRUE), "\n")
cat("Median:", median(ais_data$pedicle_width_difference_mm, na.rm = TRUE), "\n")
cat("IQR:", IQR(ais_data$pedicle_width_difference_mm, na.rm = TRUE), "\n")
# pedicle_width_convex_mm: Pedicle width on the convex side of the PT curve.
# pedicle_width_difference_mm: pedicle_width_concave_mm - pedicle_width_convex_mm. This is likely the primary outcome variable.
# vertebra_level_numeric: Numeric representation of vertebra_level (1 for T1, 2 for T2, ..., 6 for T6).

# --- Example Data Generation (for illustration purposes) ---
# This is a placeholder for how you might generate or load your data.
# In a real scenario, you would load your actual CT measurement data.

# library(tidyverse) # Or just base R for data manipulation

# set.seed(123) # for reproducibility

# num_patients <- 300
# num_vertebrae <- 6 # T1 to T6
# patient_ids <- paste0("P", str_pad(1:num_patients, 3, pad = "0"))
# vertebra_levels <- c("T1", "T2", "T3", "T4", "T5", "T6")

# # Simulate data
# ais_data <- expand.grid(patient_id = patient_ids, vertebra_level = vertebra_levels) %>%
#   as_tibble() %>%
#   mutate(
#     # Assuming PT curve is always concave on the right for simplicity in simulation
#     pt_curve_concave_side = "Right",
#     # Simulate pedicle widths (mm) with some variations
#     # Base widths for left/right for each level, then add patient and measurement noise
#     # T2 generally wider, T3/T4 right (concave) narrower
#     base_width_left = case_when(
#       vertebra_level == "T2" ~ rnorm(n(), 5.0, 0.5), # T2 generally wide
#       vertebra_level %in% c("T1", "T5", "T6") ~ rnorm(n(), 4.5, 0.5),
#       vertebra_level %in% c("T3", "T4") ~ rnorm(n(), 4.0, 0.5) # T3/T4 left (convex) might be wider than right (concave)
#     ),
#     base_width_right = case_when(
#       vertebra_level == "T2" ~ rnorm(n(), 5.0, 0.5), # T2 generally wide
#       vertebra_level %in% c("T1", "T5", "T6") ~ rnorm(n(), 4.5, 0.5),
#       vertebra_level %in% c("T3", "T4") ~ rnorm(n(), 3.5, 0.5) # T3/T4 right (concave) narrower
#     ),
#     pedicle_width_left_mm = pmax(3.0, base_width_left + rnorm(n(), 0, 0.3)), # Ensure positive widths
#     pedicle_width_right_mm = pmax(3.0, base_width_right + rnorm(n(), 0, 0.3)) # Ensure positive widths
#   ) %>%
#   # Derive concave/convex widths
#   rowwise() %>%
#   mutate(
#     pedicle_width_concave_mm = ifelse(pt_curve_concave_side == "Right", pedicle_width_right_mm, pedicle_width_left_mm),
#     pedicle_width_convex_mm = ifelse(pt_curve_concave_side == "Right", pedicle_width_left_mm, pedicle_width_right_mm),
#     pedicle_width_difference_mm = pedicle_width_concave_mm - pedicle_width_convex_mm,
#     vertebra_level_numeric = as.integer(str_replace(vertebra_level, "T", ""))
#   ) %>%
#   ungroup()

# # Select relevant columns for Stan and convert to list
# stan_data <- list(
#   N = nrow(ais_data),
#   num_vertebrae_levels = length(vertebra_levels),
#   vertebra_level_numeric = ais_data$vertebra_level_numeric,
#   pedicle_width_diff = ais_data$pedicle_width_difference_mm
# )

# # For hierarchical model, you might also include patient_id_numeric if needed
# # ais_data <- ais_data %>%
# #   mutate(patient_id_numeric = as.integer(factor(patient_id)))
# # stan_data$patient_id_numeric <- ais_data$patient_id_numeric
