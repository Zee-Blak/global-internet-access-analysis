# Global Internet Access Analysis (2020)
# Author: YOUR NAME
# R tidyverse workflow

library(tidyverse)

# -----------------------------
# 1. Load datasets
# -----------------------------
# NOTE:
# DataCamp does not allow dataset downloads.
# Replace these file paths with your local CSVs.

internet <- read_csv("data/internet_users.csv")
adoption <- read_csv("data/adoption.csv")

# -----------------------------
# 2. Filter for 2020 & countries only
# -----------------------------
internet_2020 <- internet %>%
  filter(year == 2020, !is.na(code))

adoption_2020 <- adoption %>%
  filter(year == 2020, !is.na(code))

# -----------------------------
# 3. World Bank Region Lookup
# -----------------------------
region_lookup <- tribble(
  ~code, ~region,
  "ARG","Latin America & Caribbean","BRA","Latin America & Caribbean",
  "CHL","Latin America & Caribbean","COL","Latin America & Caribbean",
  "MEX","Latin America & Caribbean","USA","North America",
  "CAN","North America","FRA","Europe & Central Asia",
  "DEU","Europe & Central Asia","GBR","Europe & Central Asia",
  "ESP","Europe & Central Asia","ITA","Europe & Central Asia",
  "SWE","Europe & Central Asia","NOR","Europe & Central Asia",
  "CHN","East Asia & Pacific","KOR","East Asia & Pacific",
  "JPN","East Asia & Pacific","AUS","East Asia & Pacific",
  "NZL","East Asia & Pacific","IND","South Asia",
  "PAK","South Asia","BGD","South Asia","LKA","South Asia",
  "NGA","Sub-Saharan Africa","GHA","Sub-Saharan Africa",
  "KEN","Sub-Saharan Africa","ZAF","Sub-Saharan Africa",
  "ETH","Sub-Saharan Africa","EGY","Middle East & North Africa",
  "MAR","Middle East & North Africa","SAU","Middle East & North Africa",
  "ARE","Middle East & North Africa","TUR","Middle East & North Africa"
)

internet_2020_region <- internet_2020 %>%
  left_join(region_lookup, by = "code")

# -----------------------------
# 4. Top 5 countries by usage
# -----------------------------
top5_countries <- internet_2020 %>%
  arrange(desc(share)) %>%
  slice(1:5)

# Save plot
ggplot(top5_countries, aes(x = reorder(entity, share), y = share)) +
  geom_col(fill = "#2C7FB8") +
  coord_flip() +
  labs(title = "Top 5 Countries by Internet Usage (2020)",
       x = "Country", y = "Internet Usage Share (%)") +
  theme_minimal()

ggsave("plots/top5_internet_usage.png", width = 8, height = 5)

# -----------------------------
# 5. Top 5 countries per region
# -----------------------------
top5_per_region <- internet_2020_region %>%
  group_by(region) %>%
  slice_max(order_by = share, n = 5) %>%
  ungroup()

ggplot(top5_per_region,
       aes(x = reorder(entity, share), y = share, fill = region)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ region, scales = "free") +
  labs(title = "Top 5 Countries per Region (2020)",
       x = "Country", y = "Internet Share (%)") +
  theme_minimal()

ggsave("plots/top5_per_region.png", width = 10, height = 8)

# -----------------------------
# 6. Correlation analysis
# -----------------------------
merged_2020 <- internet_2020 %>%
  inner_join(adoption_2020, by = c("entity", "code", "year"))

cor_result <- cor(merged_2020$share,
                  merged_2020$fixed_broadband_subs_share,
                  use = "complete.obs")

print(cor_result)

ggplot(merged_2020,
       aes(x = fixed_broadband_subs_share, y = share)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Correlation: Broadband vs Internet Usage (2020)",
       x = "Broadband Subscription Share (%)",
       y = "Internet Usage Share (%)") +
  theme_minimal()

ggsave("plots/correlation_scatter.png", width = 8, height = 6)

