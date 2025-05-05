# ==========================
# 1. Ladda nödvändiga paket
# ==========================
if (!require(readxl)) install.packages("readxl")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(leaps)) install.packages("leaps")

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaps)

# ====================================
# 2. Läs in data från Excel-fil via URL
# ====================================
url <- "https://raw.githubusercontent.com/TahiraRaza/kunskapskontroll_R-programmering/main/datainsamling_blocket_2.xlsx"
download.file(url, destfile = "datainsamling_blocket.xlsx", mode = "wb")
data <- read_excel("datainsamling_blocket.xlsx")

# ============================
# 3. Förbered och rensa data
# ============================
# Konvertera kolumner till rätt typ
data <- data %>%
  mutate(
    Försäljningspris = as.numeric(Försäljningspris),
    Miltal = as.numeric(Miltal),
    Hästkrafter = as.numeric(Hästkrafter),
    Motorstorlek = as.numeric(Motorstorlek),
    Växellåda = as.factor(Växellåda),
    Biltyp = as.factor(Biltyp),
    Drivning = as.factor(Drivning),
    Färg = as.factor(Färg),
    Bränsle = as.factor(Bränsle),
    Modell = as.factor(Modell),
    Märke = as.factor(Märke)
  )

# Städa textkategorier
data <- data %>%
  mutate(
    Bränsle = tolower(trimws(as.character(Bränsle))),
    Bränsle = recode(Bränsle,
                     "miljöbränse/hybrid" = "miljöbränsle/hybrid",
                     "miljöbränsle/ hybrid" = "miljöbränsle/hybrid"),
    Biltyp = tolower(trimws(as.character(Biltyp))),
    Färg = tolower(trimws(as.character(Färg))),
    Färg = recode(Färg,
                  "grå (grafitgrå metallic)" = "grå",
                  "grå (grå)" = "grå",
                  "grå (sandmet)" = "grå",
                  "ljusgrå" = "grå",
                  "mörkgrå" = "grå",
                  "mörkgrå (grå)" = "grå",
                  "mörkblå" = "blå",
                  "ljusblå" = "blå")
  ) %>%
  mutate(across(c(Bränsle, Biltyp, Färg), as.factor))

# ================================
# 4. Visualisera nyckelvariabler
# ================================
# Long-format för numeriska variabler
long_data <- data %>%
  pivot_longer(cols = c(Försäljningspris, Miltal, Modellår),
               names_to = "Variabel", values_to = "Värde")

# Boxplots
ggplot(long_data, aes(x = Variabel, y = Värde)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Låddiagram för numeriska variabler", x = "Variabel", y = "Värde")

ggplot(data, aes(x = Växellåda, y = Försäljningspris)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Försäljningspris per Växellåda", x = "Växellåda", y = "Försäljningspris")

ggplot(data, aes(x = Bränsle, y = Försäljningspris)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  theme_minimal() +
  labs(title = "Försäljningspris per Bränsle", x = "Bränsle", y = "Försäljningspris")

# ================================
# 5. Best Subset Selection (BIC)
# ================================

# Skapa formel
modell_formel <- Försäljningspris ~ Modellår + Miltal + Växellåda + Bränsle +
  Hästkrafter + Drivning + Motorstorlek + Biltyp

# Filtrera bort rader med NA i någon relevant variabel
modell_data <- data %>%
  select(all_of(all.vars(modell_formel))) %>%
  na.omit()

# Skapa modellmatris och målvariabel
X <- model.matrix(modell_formel, data = modell_data)[, -1]  # ta bort intercept
y <- modell_data$Försäljningspris

# Kör best subset selection
subset_model <- regsubsets(X, y, nvmax = 15, really.big = TRUE)
model_summary <- summary(subset_model)

# Visa BIC-graf
plot(subset_model, scale = "bic", main = "Best Subset Selection (BIC)")

# ================================
# 6. Bygg och utvärdera modellen
# ================================
# Hämta bästa modellens variabler
best_vars_logical <- model_summary$which[which.min(model_summary$bic), -1]  # utan intercept
best_vars <- names(best_vars_logical)[best_vars_logical]

# Skapa formel
final_formula <- as.formula(paste("y ~", paste(best_vars, collapse = " + ")))

# Bygg modell
model <- lm(final_formula, data = as.data.frame(X))

# Visa modellens sammanfattning
summary(model)

# ============================================================
# 7. Analys och diagnostisk utvärdering av regressionsmodellen
# ============================================================

# Konfidensintervall
confint(model)

# Multikollinearitet (VIF)
if (!require(car)) install.packages("car")
library(car)
vif(model)

# Residuals vs Fitted
plot(model, which = 1)

# Normal Q-Q
plot(model, which = 2)

# Scale-Location
plot(model, which = 3)

# Residuals vs Leverage
plot(model, which = 5)

# Shapiro-Wilk test för normalitet (rester)
shapiro.test(residuals(model))

# Homoskedasticitet (Breusch-Pagan test)
if (!require(lmtest)) install.packages("lmtest")
if (!require(sandwich)) install.packages("sandwich")
library(lmtest)
library(sandwich)
bptest(model)

# Robusta standardfel (vid heteroskedasticitet)
coeftest(model, vcov. = vcovHC(model, type = "HC1"))
