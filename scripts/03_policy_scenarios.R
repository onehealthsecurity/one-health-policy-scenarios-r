# ------------------------------------------------------------
# File: 03_policy_scenarios.R
# Description:
# Applies simple policy intervention scenarios to the
# One Health dataset and compares resulting risk scores.
# ------------------------------------------------------------

library(dplyr)

# Load original data
data <- read.csv("data/processed/one_health_sample_data.csv")

# Normalise helper
normalise <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Risk function
calculate_risk <- function(df) {
  df_norm <- df %>%
    mutate(
      livestock_density = normalise(livestock_density),
      wildlife_contact = normalise(wildlife_contact),
      water_quality_risk = normalise(water_quality_risk),
      vaccination_coverage = 1 - normalise(vaccination_coverage),
      veterinary_access = 1 - normalise(veterinary_access),
      human_exposure = normalise(human_exposure),
      biosecurity_score = 1 - normalise(biosecurity_score)
    )

  df_norm$risk_score <- with(df_norm,
    0.2 * livestock_density +
    0.2 * wildlife_contact +
    0.15 * water_quality_risk +
    0.15 * vaccination_coverage +
    0.1 * veterinary_access +
    0.1 * human_exposure +
    0.1 * biosecurity_score
  )

  df_norm
}

# Baseline
baseline <- calculate_risk(data) %>%
  mutate(scenario = "Baseline")

# Improved biosecurity scenario
biosecurity <- data %>%
  mutate(biosecurity_score = pmin(biosecurity_score + 15, 100)) %>%
  calculate_risk() %>%
  mutate(scenario = "Improved Biosecurity")

# Vaccination expansion scenario
vaccination <- data %>%
  mutate(vaccination_coverage = pmin(vaccination_coverage + 15, 100)) %>%
  calculate_risk() %>%
  mutate(scenario = "Vaccination Expansion")

# Veterinary access scenario
vet_access <- data %>%
  mutate(veterinary_access = pmin(veterinary_access + 15, 100)) %>%
  calculate_risk() %>%
  mutate(scenario = "Veterinary Access Expansion")

# Integrated One Health intervention
integrated <- data %>%
  mutate(
    biosecurity_score = pmin(biosecurity_score + 10, 100),
    vaccination_coverage = pmin(vaccination_coverage + 10, 100),
    veterinary_access = pmin(veterinary_access + 10, 100)
  ) %>%
  calculate_risk() %>%
  mutate(scenario = "Integrated One Health Intervention")

# Combine all scenarios
scenario_results <- bind_rows(
  baseline,
  biosecurity,
  vaccination,
  vet_access,
  integrated
)

# Save outputs
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
write.csv(scenario_results, "outputs/policy_scenario_results.csv", row.names = FALSE)

print("Policy scenario analysis complete.")
