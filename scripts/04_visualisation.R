# ------------------------------------------------------------
# File: 04_visualisation.R
# Description:
# Creates simple visualisations for One Health policy scenario
# results, comparing regional risk across intervention scenarios.
# ------------------------------------------------------------

library(ggplot2)
library(dplyr)

# Load scenario results
data <- read.csv("outputs/policy_scenario_results.csv")

# Create output folder if needed
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# Average risk by scenario
scenario_summary <- data %>%
  group_by(scenario) %>%
  summarise(mean_risk = mean(risk_score), .groups = "drop")

# Plot 1: mean risk by scenario
p1 <- ggplot(scenario_summary, aes(x = reorder(scenario, mean_risk), y = mean_risk)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Risk by Policy Scenario",
    x = "Scenario",
    y = "Mean Risk Score"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/mean_risk_by_scenario.png",
  plot = p1,
  width = 8,
  height = 5
)

# Plot 2: regional risk by scenario
p2 <- ggplot(data, aes(x = region, y = risk_score, fill = scenario)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Regional Risk Scores Across Policy Scenarios",
    x = "Region",
    y = "Risk Score"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/regional_risk_by_scenario.png",
  plot = p2,
  width = 10,
  height = 6
)

print("Visualisations created.")
