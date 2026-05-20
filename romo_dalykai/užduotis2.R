library(tidyverse)
library(ggrepel)
library(readxl) 

# Remember to set your working directory to where your files are:
# setwd("C:/Users/YourUsername/Downloads")

# ==========================================
# 1. LOAD AND CLEAN THE DATA (2013 FOCUS)
# ==========================================

# Read the Excel file, skipping the metadata text rows.
eurostat_raw <- read_excel("satisfaction.xlsx", sheet = "Sheet 1", skip = 13, col_names = FALSE)

# Clean the Eurostat data mapping explicitly to 2013 columns (B and D)
eurostat_clean <- eurostat_raw %>%
  # ...1 is Country, ...2 is Rec Areas 2013 (B), ...4 is Relationships 2013 (D)
  select(Country = ...1, Rec_Areas_2013 = ...2, Relationships_2013 = ...4) %>%
  # Filter out the header artifacts left over from skipping lines
  filter(Country != "GEO (Labels)" & !is.na(Country)) %>%
  # Convert ratings to numeric, safely turning Eurostat's ":" symbol into NA
  mutate(
    Rec_Areas_2013 = as.numeric(na_if(as.character(Rec_Areas_2013), ":")),
    Relationships_2013 = as.numeric(na_if(as.character(Relationships_2013), ":"))
  ) %>%
  # Remove rows with missing data or regional averages
  filter(!is.na(Rec_Areas_2013) & !is.na(Relationships_2013)) %>%
  filter(!Country %in% c("European Union - 27 countries (from 2020)", "Euro area – 20 countries (2023-2025)"))

# Load the OECD file (CSV format)
oecd_raw <- read_csv("social_support.csv", skip = 2)

# Clean OECD columns and standardize country names to match Eurostat formatting
oecd_clean <- oecd_raw %>%
  rename(
    Country = Category, 
    Social_Support_Pct = `Social support`
  ) %>%
  mutate(Country = case_when(
    Country == "Czech Rep." ~ "Czechia",
    Country == "Slovak Rep." ~ "Slovakia",
    TRUE ~ Country
  ))


# ==========================================
# GRAPH 1: Social Infrastructure Quadrant Matrix (2013)
# ==========================================

# Calculate the averages to position the matrix crosshairs for 2013 data
x_mid <- mean(eurostat_clean$Rec_Areas_2013, na.rm = TRUE)
y_mid <- mean(eurostat_clean$Relationships_2013, na.rm = TRUE)

ggplot(eurostat_clean, aes(x = Rec_Areas_2013, y = Relationships_2013)) +
  geom_vline(xintercept = x_mid, linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_hline(yintercept = y_mid, linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_point(color = "#2ca02c", size = 3.5, alpha = 0.8) +
  geom_text_repel(aes(label = Country), size = 3.5, max.overlaps = 15) +
  # Text quadrants labels - repositioned and centered perfectly
  annotate("text", x = x_mid + 0.8, y = y_mid + 0.6, label = "High Infrastructure\nHigh Connection", color = "#1b4d3e", fontface = "italic", size = 3.5, hjust = 0.5) +
  annotate("text", x = x_mid - 0.8, y = y_mid - 0.6, label = "Low Infrastructure\nLow Connection", color = "#8b0000", fontface = "italic", size = 3.5, hjust = 0.5) +
  annotate("text", x = x_mid - 0.8, y = y_mid + 0.6, label = "Low Infrastructure\nHigh Connection", color = "#4a69bd", fontface = "italic", size = 3.5, hjust = 0.5) +
  annotate("text", x = x_mid + 0.8, y = y_mid - 0.6, label = "High Infrastructure\nLow Connection", color = "#e67e22", fontface = "italic", size = 3.5, hjust = 0.5) + 
  labs(
    title = "Social Infrastructure Matrix (2013)",
    subtitle = "Categorizing countries by physical environments and human relationship scores",
    x = "Satisfaction with Recreational & Green Areas (0-10)",
    y = "Satisfaction with Personal Relationships (0-10)",
    caption = "Data Source: Eurostat | Dashed lines indicate data averages"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

ggsave("graph1_quadrant_matrix_2013.png", width = 9, height = 7, dpi = 300)


# ==========================================
# GRAPH 2: The Safety Net Scatter Plot (2013)
# ==========================================

# Merge the clean datasets together
merged_data <- inner_join(eurostat_clean, oecd_clean, by = "Country")

graph2 <- ggplot(merged_data, aes(x = Social_Support_Pct, y = Relationships_2013)) +
  geom_point(color = "#d62728", size = 3.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#4a69bd", linetype = "solid") +
  geom_text_repel(aes(label = Country), size = 3.5, max.overlaps = 15) +
  labs(
    title = "The Crisis Safety Net vs. Relationship Satisfaction (2013)",
    subtitle = "Does a higher percentage of crisis support map to better qualitative everyday relationships?",
    x = "OECD Social Support Metric (% of population with a crisis safety net)",
    y = "Eurostat Personal Relationship Satisfaction (0-10)",
    caption = "Data Source: Eurostat (2013) & OECD Well-being Database"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

ggsave("graph2_safety_net_scatter_2013.png", width = 9, height = 7, dpi = 300)

