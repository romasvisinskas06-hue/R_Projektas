
library(ggplot2)
library(dplyr)
#Skurdo rizikos 50 medianos (Eurostat) ir skurdo rizika pagal disposable income 50% medianos (OECD)
#https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CSociety%23SOC%23%7CInequality%23SOC_INE%23&pg=0&fc=Topic&bp=true&snb=2&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_WISE_IDD%40DF_IDD&df[ag]=OECD.WISE.INE&df[vs]=1.0&pd=2010%2C2023&dq=LTU.A.PR_INC_DISP..PT_POP._T.METH2012.D_CUR.PL_50&to[TIME_PERIOD]=false&vw=tb
#https://ec.europa.eu/eurostat/databrowser/view/ilc_li02__custom_21526880/default/table

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

grafiko_duomenys <- rbind(es_clean, disp_clean)

grafiko_duomenys <- grafiko_duomenys[order(grafiko_duomenys$Metai), ]


ggplot(grafiko_duomenys, aes(x = Metai, y = Skurdo_lygis, color = Saltinis, shape = Saltinis, group = Saltinis)) +
  geom_line(linewidth = 1) +            
  geom_point(size = 3) +                     
  scale_x_continuous(breaks = seq(min(grafiko_duomenys$Metai), max(grafiko_duomenys$Metai), by = 1)) +
  scale_color_manual(values = c("Eurostat (ES)" = "blue", "OECD (Disposable income)" = "orange")) + 
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

#----------------------------
#Rysys tarp atlyginimo ir busto kainu (Eurostat)
#https://ec.europa.eu/eurostat/databrowser/view/prc_hpi_a__custom_21527254/default/table
#https://ec.europa.eu/eurostat/databrowser/view/earn_nt_net__custom_21527250/default/table

housing_data <- read.csv("Housing.csv", stringsAsFactors = FALSE)
earnings_data <- read.csv("Net_Earnings.csv", stringsAsFactors = FALSE)

housing_clean <- housing_data[, c("TIME_PERIOD", "OBS_VALUE")]
colnames(housing_clean) <- c("Metai", "Busto_Indeksas")

earnings_clean <- earnings_data[, c("TIME_PERIOD", "OBS_VALUE")]
colnames(earnings_clean) <- c("Metai", "Uzmokestis")

bendri_duomenys <- merge(housing_clean, earnings_clean, by = "Metai")

ggplot(bendri_duomenys, aes(x = Uzmokestis, y = Busto_Indeksas)) +
  geom_point(size = 3, color = "blue") +                # Paprasti mėlyni taškai
  geom_smooth(method = "lm", color = "black", se = FALSE) + # Juoda tendencijos linija
  geom_text(aes(label = Metai), vjust = -1, size = 3) + # Metai virš taškų
  
  ylim(min(bendri_duomenys$Busto_Indeksas), max(bendri_duomenys$Busto_Indeksas) * 1.15) + 
  
  labs(
    title = "Atlyginimų ir būsto kainų ryšys Lietuvoje",
    x = "Metinis grynasis užmokestis (Eur)",
    y = "Būsto kainų indeksas (2015 = 100)"
  ) +
  theme_minimal()



#------------------------
#Lietuvos pirkimo galios palyginimas su ES vidurkiu(EuroStat)
#https://ec.europa.eu/eurostat/databrowser/view/tec00114__custom_21527436/default/table

raw_data <- read.csv("Purch_power.csv", stringsAsFactors = FALSE)

duomenys_lt <- raw_data %>%
  filter(geo == "LT") %>%
  select(Metai = TIME_PERIOD, Reiksme = OBS_VALUE) %>%
  mutate(Metai = as.factor(Metai))

ggplot(duomenys_lt, aes(x = Metai, y = Reiksme)) +
  geom_col(fill = "blue", width = 0.6) +
  
  geom_text(aes(label = Reiksme), 
            vjust = -0.5, 
            size = 3.8, 
            fontface = "bold", 
            color = "black") +
  
  geom_hline(yintercept = 100, linetype = "dashed", color = "red", linewidth = 1) +
  
  annotate("text", x = 1.8, y = 103, 
           label = "ES-27 vidurkis = 100", 
           color = "darkred", 
           fontface = "bold", 
           size = 4) +
  
  labs(
    title = "Lietuvos gyventojų perkamosios galios konvergencija",
    subtitle = "Realiojo pragyvenimo lygio (išlaidų) indeksas vienam gyventojui lyginant su ES vidurkiu",
    x = "Metai",
    y = "Indeksas (procentais nuo ES vidurkio)"
  ) +
  
  ylim(0, 115) +
  
  theme_minimal()

    #Pajamų Grupės pagal išsilavinimą OECD duomenys: https://data-explorer.oecd.org/vis?pg=0&bp=true&snb=40&tm=historical%20population%20data&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_EAG_LSO_EA%40DF_LSO_EARN_DISTR_MEDIAN&df[ag]=OECD.EDU.IMEP&df[vs]=1.0&dq=LTU._T.Y25T64.ISCED11A_0T2%2BISCED11A_3_4%2BISCED11A_5T8...MIGT_100%2BMILE_50%2BMIGT_50_LE100%2BMIGT_100_LE150%2BMIGT_150_LE200%2BMIGT_200....EMP...OBS...A&lom=LASTNOBSERVATIONS&lo=1&pd=2022%2C2022&to[TIME_PERIOD]=true&vw=tb
    
    
    
    df <- read.csv("issilavinimas.csv")
  
    ggplot(df, aes(x = ATTAINMENT_LEV, y = OBS_VALUE, fill = INCOME)) +
      geom_col() +
      
      scale_x_discrete(labels = c(
        "ISCED11A_0T2" = "Žemasis",
        "ISCED11A_3_4" = "Vidutinis",
        "ISCED11A_5T8" = "Aukštasis" 
      )) +
      
      scale_fill_discrete(labels = c(
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
