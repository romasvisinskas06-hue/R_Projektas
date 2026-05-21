pacman::p_load(pacman, readxl, ggplot2, tidyr, readr, dplyr, lubridate, eurostat)


# 1 GRAFIKAS

issilavinimas <- read.csv("issilavinimas.csv")

# 2 GRAFIKAS

pragyvenimo_lygis <- read.csv("Purch_power.csv", stringsAsFactors = FALSE)

pragyvenimo_lygis <- pragyvenimo_lygis %>%
  filter(geo == "LT") %>%
  select(Metai = TIME_PERIOD, Reiksme = OBS_VALUE) %>%
  mutate(Metai = as.factor(Metai))

# 3 GRAFIKAS

es_data <- read.csv("Skurdo_rizika_ES.csv", stringsAsFactors = FALSE)
disp_data <- read.csv("Skurdo_rizika_disposable.csv", stringsAsFactors = FALSE)

es_clean <- data.frame(
  Metai = es_data$TIME_PERIOD,
  Skurdo_lygis = es_data$OBS_VALUE,
  Saltinis = "Eurostat (ES)"
)

disp_clean <- data.frame(
  Metai = disp_data$TIME_PERIOD,
  Skurdo_lygis = disp_data$OBS_VALUE,
  Saltinis = "OECD (Disposable income)"
)

skurdas_data <- rbind(es_clean, disp_clean)

skurdas_data <- skurdas_data[order(skurdas_data$Metai),]

# 4 GRAFIKAS

gyvenimo_pasitenkinimas <- read_excel("ES gyvenimo pasitenkinimas 1 grafikas.xlsx", 
                                      sheet = "Sheet 1", 
                                      skip = 12
)
skurdo_rizika <- read_excel("Skurdo rizika 1 grafikas.xlsx", 
                            sheet = "Sheet 1", 
                            skip = 12
)

names(gyvenimo_pasitenkinimas)[1] <- "Salis"
names(skurdo_rizika)[1] <- "Salis"

es_salys <- c(
  "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany",
  "Estonia", "Ireland", "Greece", "Spain", "France",
  "Croatia", "Italy", "Cyprus", "Latvia", "Lithuania",
  "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",
  "Poland", "Portugal", "Romania", "Slovenia", "Slovakia",
  "Finland", "Sweden"
)

life <- gyvenimo_pasitenkinimas %>%
  select(Salis, `2024`) %>%
  filter(Salis %in% es_salys) %>%
  mutate(Gyvenimo_pasitenkinimas = as.numeric(`2024`)) %>%
  select(Salis, Gyvenimo_pasitenkinimas)

poverty <- skurdo_rizika %>%
  select(Salis, `17.3`) %>%
  filter(Salis %in% es_salys) %>%
  mutate(Skurdo_rizika = as.numeric(`17.3`)) %>%
  select(Salis, Skurdo_rizika)

duomenys_kor <- inner_join(life, poverty, by = "Salis") %>%
  filter(
    !is.na(Gyvenimo_pasitenkinimas),
    !is.na(Skurdo_rizika)
  ) %>%
  mutate(Grupe = ifelse(Salis == "Lithuania", "Lietuva", "Kitos ES šalys"))

r <- cor(
  duomenys_kor$Skurdo_rizika,
  duomenys_kor$Gyvenimo_pasitenkinimas,
  use = "complete.obs"
)


# 5 GRAFIKAS

material_deprivation <- get_eurostat("ilc_mdes04")

material_deprivation <- material_deprivation %>%
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






# 6 GRAFIKAS

pajamos <- get_eurostat("ilc_di03")
busto_kainos <- get_eurostat("prc_hpi_a")
infliacijos_indeksas <- get_eurostat("prc_hicp_aind")  # nuomos indeksas

pajamos <- pajamos %>%
  filter(
    geo      == "LT",
    unit     == "EUR",
    sex      == "T",
    age      == "TOTAL",
    as.integer(format(TIME_PERIOD, "%Y")) >= 2015,
    as.integer(format(TIME_PERIOD, "%Y")) <= 2023
  ) %>%
  mutate(metai = as.integer(format(TIME_PERIOD, "%Y"))) %>%
  select(metai, pajamos = values)

busto_kainos <- busto_kainos %>%
  filter(
    geo      == "LT",
    purchase == "TOTAL",
    unit     == "I15_A_AVG",
    as.integer(format(TIME_PERIOD, "%Y")) >= 2015,
    as.integer(format(TIME_PERIOD, "%Y")) <= 2023
  ) %>%
  mutate(metai = as.integer(format(TIME_PERIOD, "%Y"))) %>%
  select(metai, hpi = values)

infliacijos_indeksas <- infliacijos_indeksas %>%
  filter(
    geo    == "LT",
    coicop == "CP041",
    unit   == "INX_A_AVG",  # metinis indeksas
    as.integer(format(TIME_PERIOD, "%Y")) >= 2015,
    as.integer(format(TIME_PERIOD, "%Y")) <= 2023
  ) %>%
  mutate(metai = as.integer(format(TIME_PERIOD, "%Y"))) %>%
  select(metai, nuoma = values)


df1 <- pajamos %>%
  inner_join(busto_kainos,  by = "metai") %>%
  inner_join(infliacijos_indeksas, by = "metai") %>%
  arrange(metai) %>%
  mutate(
    pajamos_idx = pajamos / pajamos[1] * 100,
    busto_idx     = hpi     / hpi[1]     * 100,
    nuoma_idx   = nuoma   / nuoma[1]   * 100
  ) %>%
  pivot_longer(
    cols      = c(pajamos_idx, busto_idx, nuoma_idx),
    names_to  = "rodiklis",
    values_to = "indeksas"
  ) %>%
  mutate(rodiklis = recode(rodiklis,
                           "pajamos_idx" = "Vidutinės pajamos",
                           "busto_idx"     = "Būsto kainų indeksas",
                           "nuoma_idx"   = "Nuomos indeksas"
  ))


# 7 GRAFIKAS

perpildymas <- get_eurostat("ilc_lvho05a")

perpildymas <- perpildymas %>%
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
  select(metai, salis, proc = values) %>%
  group_by(metai) %>%
  filter(n() == 2) %>%
  ungroup()

# 8 GRFIKAS
happiness <- read_csv("happiness-cantril-ladder.csv")

happiness <- happiness %>%
  filter(Year >= 2013, Year <= 2025)

es_salys <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
  "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
  "Slovenia", "Spain", "Sweden"
)

lietuva <- happiness %>%
  filter(Entity == "Lithuania") %>%
  transmute(
    Year,
    Reiksme = `Self-reported life satisfaction`,
    Grupe = "Lietuva"
  )

es_vidurkis <- happiness %>%
  filter(Entity %in% es_salys) %>%
  group_by(Year) %>%
  summarise(Reiksme = mean(`Self-reported life satisfaction`, na.rm = TRUE)) %>%
  mutate(Grupe = "ES vidurkis")

pasaulio_vidurkis <- happiness %>%
  filter(!(Entity %in% es_salys)) %>%
  group_by(Year) %>%
  summarise(Reiksme = mean(`Self-reported life satisfaction`, na.rm = TRUE)) %>%
  mutate(Grupe = "Pasaulio vidurkis be ES šalių")

happiness_data <- bind_rows(lietuva, es_vidurkis, pasaulio_vidurkis)

# 9 GRAFIKAS

eurostat <- read_excel("satisfaction.xlsx", sheet = "Sheet 1", skip = 13, col_names = FALSE)

eurostat <- eurostat %>%
  select(Country = ...1, zalios_erdves = ...2, santykiai = ...4) %>%
  filter(Country != "GEO (Labels)" & !is.na(Country)) %>%
  mutate(
    zalios_erdves = as.numeric(na_if(as.character(zalios_erdves), ":")),
    santykiai = as.numeric(na_if(as.character(santykiai), ":"))
  ) %>%
  filter(!is.na(zalios_erdves) & !is.na(santykiai)) %>%
  filter(!Country %in% c("European Union - 27 countries (from 2020)", "Euro area – 20 countries (2023-2025)"))

oecd <- read_csv("social_support.csv", skip = 2)

oecd <- oecd %>%
  rename(
    Country = Category, 
    social_support = `Social support`
  ) %>%
  mutate(Country = case_when(
    Country == "Czech Rep." ~ "Czechia",
    Country == "Slovak Rep." ~ "Slovakia",
    TRUE ~ Country
  ))

x_vid <- mean(eurostat$zalios_erdves, na.rm = TRUE)
y_vid <- mean(eurostat$santykiai, na.rm = TRUE)
