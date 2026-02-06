# ==========================================
# Title: Kenya Maternal Health Analysis
# Goal: Showcase Reproducibility & Collaboration
# ==========================================

# 1. SETUP: Create necessary project folders
# This ensures the script never crashes due to missing directories
folders <- c("data", "scripts", "output")
for (f in folders) {
  if (!dir.exists(f)) {
    dir.create(f)
  }
}

# 2. LOAD LIBRARIES
# If these aren't installed, use: install.packages(c("dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)

# 3. DATA SIMULATION (Reproducibility Step)
set.seed(254) # Ensuring the "random" data is the same for everyone

counties <- c("Nairobi", "Mombasa", "Kisumu", "Nakuru", "Kiambu",
              "Uasin Gishu", "Turkana", "Mandera", "Machakos", "Kilifi")

kenya_health_data <- data.frame(
  county = rep(counties, each = 5),
  year = rep(2019:2023, times = 10),
  skilled_attendants_pct = runif(50, min = 40, max = 95),
  mmr = rnorm(50, mean = 400, sd = 50)
)

# Apply a trend: as attendance increases, mortality decreases
kenya_health_data$mmr <- kenya_health_data$mmr - (kenya_health_data$skilled_attendants_pct * 2.5)

# Save the raw data
write.csv(kenya_health_data, "data/kenya_maternal_health.csv", row.names = FALSE)

View(kenya_health_data)
# 4. ANALYSIS & VISUALIZATION

# Plot A: County-wise Distribution (Boxplot)
county_dist_plot <- ggplot(kenya_health_data, aes(x = reorder(county, mmr), y = mmr, fill = county)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() +
  labs(title = "Maternal Mortality Distribution by County (2019-2023)",
       subtitle = "Comparing simulated healthcare outcomes across counties",
       x = "County", y = "MMR (per 100,000)") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot B: Time-Series Trend (Line graph with fixed linewidth)
trend_plot <- kenya_health_data %>%
  group_by(year) %>%
  summarise(avg_mmr = mean(mmr)) %>%
  ggplot(aes(x = year, y = avg_mmr)) +
  geom_line(linewidth = 1.2, color = "#008080") + # Corrected aesthetic
  geom_point(size = 3, color = "#008080") +
  labs(title = "National Average Maternal Mortality Trend",
       subtitle = "Simulated annual progress (2019-2023)",
       x = "Year", y = "Average MMR") +
  theme_light()

# 5. PREDICTION MODELING
# Simple linear regression: MMR explained by skilled attendance
model <- lm(mmr ~ skilled_attendants_pct, data = kenya_health_data)

# Predict MMR for 100% skilled attendance coverage
future_scenario <- data.frame(skilled_attendants_pct = 100)
predicted_val <- predict(model, newdata = future_scenario)

# Print results to Console
cat("\n--- Prediction Analysis ---\n")
print(paste("Predicted MMR if Skilled Attendance is 100%:", round(predicted_val, 2)))

# 6. EXPORTING OUTPUTS
ggsave("output/county_distribution.png", plot = county_dist_plot, width = 8, height = 6)
ggsave("output/national_trend.png", plot = trend_plot, width = 8, height = 6)

print("Analysis complete. All files saved to 'data/' and 'output/' folders.")
