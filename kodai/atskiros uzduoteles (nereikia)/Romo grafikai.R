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
  geom_point(data = eurostat_clean %>% filter(Country != "Lithuania"),
             color = "pink", size = 3.5, alpha = 0.8) +
  geom_text_repel(
    data = eurostat_clean %>% filter(Country != "Lithuania"),
    aes(label = Country),
    size = 3,
    max.overlaps = 15
  )+
  geom_point(
    data = eurostat_clean %>% filter(Country == "Lithuania"),
    color = "red",
    size = 4
  )+ 
  geom_text(
    data = eurostat_clean %>% filter(Country == "Lithuania"),
    aes(label = "Lithuania"),
    vjust = -1.2,
    hjust = 0.8,
    fontface = "bold",
    size = 3.7
  ) +
  xlim(5.3,8.5)+
  ylim(6.5, 9)+
  labs(
    title = "Pasitenkinimo žaliomis erdvėmis ir asmeniniais santykiais ryšys",
    x = "Pasitenkinimas rekreacinėmis ir žaliosiomis erdvėmis",
    y = "Pasitenkinimas asmeniniais santykiais"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )+
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    linetype = "dashed",
    linewidth = 1, 
    alpha = 0.6
  ) 

ggsave("graph1_quadrant_matrix_2013.png", width = 9, height = 7, dpi = 300)

