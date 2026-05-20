library(readr)
library(dplyr)
library(ggplot2)
library(readxl)

gyvenimo_pasitenkinimas <- read_excel(
  "~/R programavimas/ES gyvenimo pasitenkinimas 1 grafikas.xlsx", 
  sheet = "Sheet 1", 
  skip = 12
)

skurdo_rizika <- read_excel(
  "~/R programavimas/Skurdo rizika 1 grafikas.xlsx", 
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

ggplot(duomenys_kor, aes(x = Skurdo_rizika, y = Gyvenimo_pasitenkinimas)) +
  geom_point(
    data = duomenys_kor %>% filter(Salis != "Lithuania"),
    color = "grey50",
    size = 3
  ) +
  geom_point(
    data = duomenys_kor %>% filter(Salis == "Lithuania"),
    color = "red",
    size = 4
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    linetype = "dashed",
    linewidth = 1
  ) +
  geom_text(
    data = duomenys_kor %>% filter(Salis != "Lithuania"),
    aes(label = Salis),
    vjust = -0.8,
    size = 3
  ) +
  geom_text(
    data = duomenys_kor %>% filter(Salis == "Lithuania"),
    aes(label = "Lithuania"),
    vjust = -1,
    fontface = "bold",
    size = 4
  ) +
  annotate(
    "text",
    x = 10,
    y = 6.3,
    label = paste0("Koreliacija r = ", round(r, 2)),
    hjust = 0,
    size = 5,
    fontface = "bold"
  ) +
  labs(
    title = "Skurdo rizikos ir gyvenimo pasitenkinimo ryšys ES šalyse",
    subtitle = "Lietuvos padėtis ES šalių kontekste, 2024 m.",
    x = "Skurdo rizikos lygis (%)",
    y = "Gyvenimo pasitenkinimas (0–10)"
  ) +
  theme_minimal()

#2GRAFIKAS
library(readr)
duom <- read_csv("~/R programavimas/happiness-cantril-ladder.csv")

library(dplyr)
library(ggplot2)

duom <- duom %>%
  filter(Year >= 2013, Year <= 2025)

es_salys <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
  "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
  "Slovenia", "Spain", "Sweden"
)

lietuva <- duom %>%
  filter(Entity == "Lithuania") %>%
  transmute(
    Year,
    Reiksme = `Self-reported life satisfaction`,
    Grupe = "Lietuva"
  )

es_vidurkis <- duom %>%
  filter(Entity %in% es_salys) %>%
  group_by(Year) %>%
  summarise(Reiksme = mean(`Self-reported life satisfaction`, na.rm = TRUE)) %>%
  mutate(Grupe = "ES vidurkis")

pasaulio_vidurkis <- duom %>%
  filter(!(Entity %in% es_salys)) %>%
  group_by(Year) %>%
  summarise(Reiksme = mean(`Self-reported life satisfaction`, na.rm = TRUE)) %>%
  mutate(Grupe = "Pasaulio vidurkis be ES šalių")

grafiko_duom <- bind_rows(lietuva, es_vidurkis, pasaulio_vidurkis)

ggplot(grafiko_duom, aes(x = Year, y = Reiksme, color = Grupe)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2013:2025) +
  scale_y_continuous(limits = c(5, 7)) +
  labs(
    title = "Lietuvos, ES ir pasaulio šalių vidutinis laimės vertinimas",
    subtitle = "Vertinimas pateikiamas skalėje nuo 1 iki 10",
    x = "Metai",
    y = "Vertinimas nuo 1 iki 10",
    color = ""
  ) +
  theme_minimal()













