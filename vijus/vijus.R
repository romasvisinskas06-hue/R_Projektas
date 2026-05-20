happiness <- import("happiness-cantril-ladder.csv")
happiness <- happiness %>%
  select(Code ,Entity, Year, `Self-reported life satisfaction`)%>%
  pivot_wider( names_from = Year, values_from = `Self-reported life satisfaction` )
