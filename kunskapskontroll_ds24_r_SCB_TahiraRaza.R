# Steg 1: Installera och ladda nödvändiga paket
if (!require(pxweb)) install.packages("pxweb")  # För att hämta data från SCB:s API
if (!require(dplyr)) install.packages("dplyr")  # För datarensning
if (!require(ggplot2)) install.packages("ggplot2")  # För att skapa grafer
library(pxweb)
library(dplyr)
library(ggplot2)

# Steg 2: Ange API-endpoint för "Fordon i trafik"
url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/FordonTrafik"

# Steg 3: Skapa en fråga med rätt variabler
query <- list(
  "Region" = c("00"),  # Hela Sverige
  "Fordonsslag" = c("10"),  # Personbilar
  "ContentsCode" = c("TK1001AC"),  # Antal fordon
  "Tid" = as.character(2004:2024)  # Åren 2004 till 2024
)

# Steg 4: Hämta data från SCB:s API
px_result <- pxweb_get(url = url, query = query)

# Steg 5: Omvandla data till en data.frame
df <- as.data.frame(px_result, column.name.type = "code", variable.value.type = "text")

# Kontrollera kolumnnamnen i df
print(names(df))

# Steg 6: Rensa och strukturera data
df <- df %>%
  rename(
    year = Tid,  # Kolumnen för år
    number_of_cars = TK1001AC  # Kolumnen för antal bilar
  ) %>%
  mutate(
    year = as.numeric(year),  # Konvertera år till numerisk
    number_of_cars = as.numeric(number_of_cars)  # Säkerställ att antal bilar är numerisk
  )

# Kontrollera datan
print(head(df))

# Steg 7: Visualisera data med ggplot2
ggplot(df, aes(x = year, y = number_of_cars)) +
  geom_line(color = "blue", linewidth = 1) +  # Linje för antal bilar
  geom_point(color = "red", size = 2) +       # Punkter för år
  scale_x_continuous(
    breaks = seq(2004, 2024, by = 2)  # Visa endast jämna årtal
  ) +
  scale_y_continuous(
    limits = c(0, 6000000),           # Sätt y-axeln från 0 till 6 000 000
    breaks = seq(0, 6000000, by = 500000) # Intervall på 500 000
  ) +
  labs(
    title = "Antal personbilar i trafiken (2004-2024)",
    x = "År",
    y = "Antal bilar"
  ) +
  theme_minimal() +  # Enkel layout
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10)
  )