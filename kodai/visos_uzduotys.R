# ---1 GRAFIKAS---

g1 <- ggplot(issilavinimas, aes(x = ATTAINMENT_LEV, y = OBS_VALUE, fill = INCOME)) +
  geom_col() +
  scale_x_discrete(labels = c(
    "ISCED11A_0T2" = "Žemasis",
    "ISCED11A_3_4" = "Vidutinis",
    "ISCED11A_5T8" = "Aukštasis" 
  )) +
  scale_fill_manual(
    values = c(
    "MILE_50" = "salmon",
    "MIGT_50_LE100" = "#B5E7A0",
    "MIGT_150_LE200" = "pink",
    "MIGT_200" = "skyblue",
    "MIGT_100_LE150" = "#C39BD3"),
    labels = c(
    "MILE_50" = "Žemiau 50% medianos",
    "MIGT_50_LE100" = "50-100% medianos",
    "MIGT_150_LE200" = "150-200% medianos",
    "MIGT_200" = "Aukščiau 200% medianos",
    "MIGT_100_LE150" = "100-150% medianos"
    )) +
  labs(
    title = "Pajamų pasiskirstymas pagal išsilavinimą",
    x = "Išsilavinimo lygis",
    y = "Procentai (%)",
    fill = "Pajamų grupė"
  ) +
  theme_minimal()


#---2 GRAFIKAS---

g2 <- ggplot(pragyvenimo_lygis, aes(x = Metai, y = Reiksme)) +
  geom_col(fill = "skyblue", width = 0.6) +
  labs(
    title = "Realiojo pragyvenimo lygio indeksas vienam Lietuvos gyventojui lyginant su ES vidurkiu",
    x = "Metai",
    y = "Indeksas (procentais nuo ES vidurkio)"
  ) +
  ylim(0, 100) +
  theme_minimal()

#---3 GRAFIKAS---

g3 <- ggplot(skurdas_data, aes(x = Metai, y = Skurdo_lygis,
                         color = Saltinis, shape = Saltinis, 
                         group = Saltinis)) +
  geom_line(linewidth = 1) +            
  geom_point(size = 2.5) +                     
  scale_x_continuous(breaks = seq(min(skurdas_data$Metai), max(skurdas_data$Metai), by = 1)) +
  scale_color_manual(values = c("Eurostat (ES)" = "skyblue",
                                "OECD (Disposable income)" = "#C39BD3")) + 
  labs(
    title = "Skurdo rizikos lygio palyginimas Lietuvoje",
    subtitle = "Riba: 50% medianos pajamų (Eurostat vs OECD)",
    x = "Metai",
    y = "Skurdo rizikos lygis (%)",
    color = "Duomenų šaltinis",
    shape = "Duomenų šaltinis"
  ) +
  theme_minimal(base_size = 12) +           
  theme(
    plot_title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot_subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    legend.position = "bottom",     
    panel.grid.minor = element_blank(),   
    axis.text.x = element_text(angle = 45, vjust = 0.5) 
  )

#---4 GRAFIKAS---

g4 <- ggplot(duomenys_kor, aes(x = Skurdo_rizika, y = Gyvenimo_pasitenkinimas)) +
  geom_point(
    data = duomenys_kor %>% filter(!Salis %in% c("Lithuania", "Malta")),
    color = "pink",
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
    data = duomenys_kor %>% filter(!Salis %in% c("Lithuania", "Malta", "Cyprus", "Denmark")),
    aes(label = Salis),
    vjust = -0.8,
    size = 3
  ) +
  geom_text(
    data = duomenys_kor %>% filter(Salis == "Lithuania"),
    aes(label = "Lithuania"),
    vjust = -1.2,
    hjust = 0.8,
    fontface = "bold",
    size = 3.7
  ) +
  geom_text(
    data = duomenys_kor %>% filter(Salis == "Denmark"),
    aes(label = "Denmark"),
    vjust = -0.8,
    hjust = 1.2,
    size = 3
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

#---5 GRAFIKAS---

g5 <- ggplot(material_deprivation, aes(x = metai, y = proc, fill = salis)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Lietuva" = "skyblue", "ES vidurkis" = "pink")) +
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

#---6 GRAFIKAS---

g6 <- ggplot(df1, aes(x = metai, y = indeksas, color = rodiklis, group = rodiklis)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "Vidutinės pajamos"    = "skyblue",
    "Būsto kainų indeksas" = "#B39EB5",
    "Nuomos indeksas"      = "salmon"
  )) +
  scale_x_continuous(breaks = 2015:2023) +
  labs(
    title    = "Pajamos, būsto kainos ir nuoma Lietuvoje",
    x        = "Metai",
    y        = "Indeksas"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

#---7 GRAFIKAS---

g7 <- ggplot(perpildymas, aes(x = metai, y = proc, color = salis, group = salis)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Lietuva" = "salmon", "ES vidurkis" = "skyblue")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
  ylim(0,100) +
  labs(
    title = "Gyventojų dalis, gyvenančių perpildytuose būstuose",
    x        = "Metai",
    y        = "Gyventojų dalis (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 15),
    legend.position    = "bottom",
    panel.grid.minor   = element_blank()
  )



#---8 GRAFIKAS---


g8 <- ggplot(happiness_data, aes(x = Year, y = Reiksme, color = Grupe)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2013:2025) +
  scale_y_continuous(limits = c(5, 7)) +
  scale_color_manual(
    values = c(
      "Lietuva" = "skyblue",
      "ES vidurkis"        = "pink",
      "Pasaulio vidurkis be ES šalių"     = "#B39EB5"
    )
  ) +
  labs(
    title = "Lietuvos, ES ir pasaulio šalių vidutinis laimės vertinimas",
    subtitle = "Vertinimas pateikiamas skalėje nuo 1 iki 10",
    x = "Metai",
    y = "Vertinimas nuo 1 iki 10",
    color = ""
  ) +
  theme_minimal()

#---9 GRAFIKAS---

g9 <- ggplot(eurostat, aes(x = zalios_erdves, y = santykiai)) +
  geom_point(data = eurostat %>% filter(Country != "Lithuania"),
             color = "pink", size = 3.5, alpha = 0.8) +
  geom_text_repel(
    data = eurostat %>% filter(Country != "Lithuania"),
    aes(label = Country),
    size = 3,
    max.overlaps = 15
  )+
  geom_point(
    data = eurostat %>% filter(Country == "Lithuania"),
    color = "red",
    size = 4
  )+ 
  geom_text(
    data = eurostat %>% filter(Country == "Lithuania"),
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
