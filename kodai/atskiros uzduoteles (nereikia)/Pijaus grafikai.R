# ============================================================

# ============================================================
# UŽDAVINYS 1: Pajamos, būsto kainos ir nuoma Lietuvoje
# Trys linijos: vidutinės pajamos, būsto kainų indeksas, nuomos indeksas
# ============================================================

library(eurostat)
library(ggplot2)
library(dplyr)
library(tidyr)

# --- 1. Parsisiųsti ---
pajamos_raw <- get_eurostat("ilc_di03")
hpi_raw     <- get_eurostat("prc_hpi_a")
hicp_raw    <- get_eurostat("prc_hicp_aind")  # nuomos indeksas

# --- 2. Filtruoti ---

pajamos_lt <- pajamos_raw %>%
  filter(
    geo      == "LT",
    indic_il == "MED_E",
    unit     == "EUR",
    sex      == "T",
    age      == "TOTAL",
    as.integer(format(TIME_PERIOD, "%Y")) >= 2015,
    as.integer(format(TIME_PERIOD, "%Y")) <= 2023
  ) %>%
  mutate(metai = as.integer(format(TIME_PERIOD, "%Y"))) %>%
  select(metai, pajamos = values)

hpi_lt <- hpi_raw %>%
  filter(
    geo      == "LT",
    purchase == "TOTAL",
    unit     == "I15_A_AVG",
    as.integer(format(TIME_PERIOD, "%Y")) >= 2015,
    as.integer(format(TIME_PERIOD, "%Y")) <= 2023
  ) %>%
  mutate(metai = as.integer(format(TIME_PERIOD, "%Y"))) %>%
  select(metai, hpi = values)

# HICP CP041 = faktinės nuomos kainos
nuoma_lt <- hicp_raw %>%
  filter(
    geo    == "LT",
    coicop == "CP041",
    unit   == "INX_A_AVG",  # metinis indeksas
    as.integer(format(TIME_PERIOD, "%Y")) >= 2015,
    as.integer(format(TIME_PERIOD, "%Y")) <= 2023
  ) %>%
  mutate(metai = as.integer(format(TIME_PERIOD, "%Y"))) %>%
  select(metai, nuoma = values)

cat("Pajamos eilučių:", nrow(pajamos_lt), "\n")
cat("HPI eilučių:", nrow(hpi_lt), "\n")
cat("Nuoma eilučių:", nrow(nuoma_lt), "\n")

# --- 3. Sujungti ir normalizuoti (2015 = 100) ---
df1 <- pajamos_lt %>%
  inner_join(hpi_lt,  by = "metai") %>%
  inner_join(nuoma_lt, by = "metai") %>%
  arrange(metai) %>%
  mutate(
    pajamos_idx = pajamos / pajamos[1] * 100,
    hpi_idx     = hpi     / hpi[1]     * 100,
    nuoma_idx   = nuoma   / nuoma[1]   * 100
  ) %>%
  pivot_longer(
    cols      = c(pajamos_idx, hpi_idx, nuoma_idx),
    names_to  = "rodiklis",
    values_to = "indeksas"
  ) %>%
  mutate(rodiklis = recode(rodiklis,
                           "pajamos_idx" = "Vidutinės pajamos",
                           "hpi_idx"     = "Būsto kainų indeksas",
                           "nuoma_idx"   = "Nuomos indeksas"
  ))

cat("\nSujungti duomenys:\n"); print(df1)

# --- 4. Grafikas ---
p1 <- ggplot(df1, aes(x = metai, y = indeksas, color = rodiklis, group = rodiklis)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "Vidutinės pajamos"    = "#2196F3",
    "Būsto kainų indeksas" = "#F44336",
    "Nuomos indeksas"      = "#FF9800"
  )) +
  scale_x_continuous(breaks = 2015:2023) +
  labs(
    title    = "Pajamos, būsto kainos ir nuoma Lietuvoje",
    x        = "Metai",
    y        = "Indeksas",
    color    = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

print(p1)
ggsave("uzdavinys1_v2.png", p1, width = 10, height = 6, dpi = 150)
cat("\n✅ Išsaugota: uzdavinys1_v2.png\n")
# ============================================================
# UŽDAVINYS 2: Finansinis nesaugumas dėl būsto kaštų
# ilc_mdes04 – negalėjimas padengti netikėtų išlaidų
# ============================================================

mdes_raw <- get_eurostat("ilc_mdes04")

cat("=== MDES stulpeliai ===\n"); print(names(mdes_raw))
cat("\n=== MDES pirmosios eilutės ===\n"); print(head(mdes_raw, 10))
cat("\n=== hhtyp reikšmės ===\n"); print(unique(mdes_raw$hhtyp))

mdes <- mdes_raw %>%
  filter(
    geo %in% c("LT", "EU27_2020"),
    hhtyp == "TOTAL",
    as.integer(format(TIME_PERIOD, "%Y")) >= 2015,
    as.integer(format(TIME_PERIOD, "%Y")) <= 2023
  ) %>%
  mutate(
    metai = as.integer(format(TIME_PERIOD, "%Y")),
    salis = recode(geo, "LT" = "Lietuva", "EU27_2020" = "ES vidurkis")
  ) %>%
  select(metai, salis, proc = values)

cat("\n=== Filtruoti duomenys ===\n"); print(mdes)

p2 <- ggplot(mdes, aes(x = metai, y = proc, fill = salis)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Lietuva" = "#FF9800", "ES vidurkis" = "#9E9E9E")) +
  scale_x_continuous(breaks = 2015:2023) +
  ylim(0,100) +
  labs(
    title    = "Finansinis nesaugumas",
    subtitle = "Namų ūkių dalis, negalinti padengti netikėtų išlaidų",
    x        = "Metai",
    y        = "namų ūkių (%)",
    fill     = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

print(p2)
#ggsave("uzdavinys2_nesaugumas.png", p2, width = 10, height = 6, dpi = 150)
#cat("\n✅ Uždavinys 2 išsaugotas: uzdavinys2_nesaugumas.png\n")

# ============================================================
# UŽDAVINYS 3: Būsto perpildymas – Lietuva vs. ES vidurkis
# ============================================================


perpildymas_raw <- get_eurostat("ilc_lvho05a")

df3 <- perpildymas_raw %>%
  filter(
    geo    %in% c("LT", "EU27_2020"),
    unit   == "PC",
    incgrp == "TOTAL",
    sex    == "T",
    age    == "TOTAL"
  ) %>%
  mutate(
    metai = as.integer(format(TIME_PERIOD, "%Y")),
    salis = recode(geo, "LT" = "Lietuva", "EU27_2020" = "ES vidurkis")
  ) %>%
  select(metai, salis, proc = values)

# Palikti tik metus kur YRA ABU duomenys
metai_abu <- df3 %>%
  group_by(metai) %>%
  filter(n() == 2) %>%
  ungroup()

p3 <- ggplot(metai_abu, aes(x = metai, y = proc, color = salis, group = salis)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Lietuva" = "#E91E63", "ES vidurkis" = "#1565C0")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
  ylim(0,100) +
  labs(
    title = "Gyventojų dalis, gyvenančių perpildytuose būstuose",
    x        = "Metai",
    y        = "Gyventojų dalis (%)",
    color    = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 15),
    legend.position    = "bottom",
    panel.grid.minor   = element_blank()
  )

print(p3)
#ggsave("uzdavinys3_laikas_v2.png", p3, width = 10, height = 6, dpi = 150)
#cat("✅ Išsaugota: uzdavinys3_laikas_v2.png\n")


