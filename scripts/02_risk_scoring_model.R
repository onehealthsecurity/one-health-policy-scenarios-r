# ------------------------------------------------------------
# File: 02_risk_scoring_model.R
# Description:
# Processes One Health indicators and generates a composite
# zoonotic risk score using weighted variables.
# ------------------------------------------------------------

library(dplyr)

# Load data
data <- read.csv("data/processed/one_health_sample_data.csv")

# Normalise variables (0–1 scale)
normalise <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data_norm <- data %>%
  mutate(
    livestock_density = normalise(livestock_density),
    wildlife_contact = normalise(wildlife_contact),
    water_quality_risk = normalise(water_quality_risk),
    vaccination_coverage = 1 - normalise(vaccination_coverage), # lower coverage = higher risk
    veterinary_access = 1 - normalise(veterinary_access),
    human_exposure = normalise(human_exposure),
    biosecurity_score = 1 - normalise(biosecurity_score)
  )

# Weighted risk score
data_norm$risk_score <- with(data_norm,
  0.2 * livestock_density +
  0.2 * wildlife_contact +
  0.15 * water_quality_risk +
  0.15 * vaccination_coverage +
  0.1 * veterinary_access +
  0.1 * human_exposure +
  0.1 * biosecurity_score
)

# Save output
write.csv(data_norm, "data/processed/risk_scores.csv", row.names = FALSE)

print("Risk scoring complete.")
