#Temperature based Planet Sub-Type Association with Main Sequence Stars

library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(RColorBrewer)

planetsInitial <- read_csv("/Users/vgatne/Documents/PIE/Data/planetStellarProperties_noErrors.csv", skip = 19)

colnames(planetsInitial) <- c("PlanetName", "HostName", "NumStars", "NumPlanets", "OrbitalPeriod", "SemiMajorAxis", "PlanetRadius", "PlanetMass", "PlanetTemp", "SpectralType", "StellarTemp", "StellarMass", "Luminosity")

#Removes planets in multi-star systems
planetsInitial <- planetsInitial[(planetsInitial$NumStars == 1), ]

#Adding Planet Types
planetsInitial <- planetsInitial %>% mutate("PlanetType" = case_when(
  (planetsInitial$"PlanetRadius" > 0) & (planetsInitial$"PlanetRadius" < 1.7) ~ "Terrestrial",
  (planetsInitial$"PlanetRadius" >= 1.7) & (planetsInitial$"PlanetRadius" < 3.9) ~ "Mini-Neptune",
  ((planetsInitial$"PlanetRadius" >= 3.9) & (planetsInitial$"PlanetRadius" < 9.4)) & (planetsInitial$"PlanetMass" < 1) ~ "Sub-Saturn",
  (planetsInitial$"PlanetRadius" >= 9.4) | (planetsInitial$"PlanetMass" > 1) ~ "Gas Giant",
  .default = "Other"))

planetsInitial <- planetsInitial[!(planetsInitial$PlanetType == "Other"), ]

#Estimating values for missing planet temperature data
calculateAxis <- function(stellarMass, period) {(((6.67e-11 * (stellarMass * 1.989e+30) * ((period * 86400) ^ 2)) / (4 * (pi ^ 2))) ^ (1 / 3)) / 1.496e+11}

calculateTemp <- function(luminosity, axis) {(((10 ^ luminosity) * 3.827e+26) / (16 * pi * 5.67e-08 * ((axis * 1.496e+11) ^ 2))) ^ (1 / 4)}

planetsInitial$"SemiMajorAxis"[is.na(planetsInitial$"SemiMajorAxis")] <- calculateAxis(planetsInitial$"StellarMass", planetsInitial$"OrbitalPeriod")

planetsInitial$"PlanetTemp"[is.na(planetsInitial$"PlanetTemp")] <- calculateTemp(planetsInitial$"Luminosity", planetsInitial$"SemiMajorAxis")

planetsInitial$PlanetTemp[which(is.na(planetsInitial$PlanetTemp))] <- calculateTemp(planetsInitial$Luminosity, planetsInitial$SemiMajorAxis)

#Adding Planet Sub-Types based on Temperature
planetsInitial <- planetsInitial %>% mutate("PlanetTempSubType" = case_when(
  (planetsInitial$"PlanetTemp" >= 1000) ~ paste("Hot", planetsInitial$"PlanetType", sep=" "),
  (planetsInitial$"PlanetTemp" >= 400) & (planetsInitial$"PlanetTemp" < 1000) ~ paste("Warm", planetsInitial$"PlanetType", sep=" "),
  (planetsInitial$"PlanetTemp" >= 200) & (planetsInitial$"PlanetTemp" < 400) ~ paste("Temperate", planetsInitial$"PlanetType", sep=" "),
  (planetsInitial$"PlanetTemp" < 200) ~ paste("Cold", planetsInitial$"PlanetType", sep=" "),
  .default = "Other"))

planetsInitial <- planetsInitial[!(planetsInitial$PlanetTempSubType == "Other"), ]

#Adding Spectral Sub-Type and adjusting for consistency and NAs in data
planetsInitial$SpectralSubType <- case_when(
  (planetsInitial$"StellarTemp" >= 2400) & (planetsInitial$"StellarTemp" < 3700) ~ "M",
  (planetsInitial$"StellarTemp" >= 3700) & (planetsInitial$"StellarTemp" < 5200) ~ "K",
  (planetsInitial$"StellarTemp" >= 5200) & (planetsInitial$"StellarTemp" < 6000) ~ "G",
  (planetsInitial$"StellarTemp" >= 6000) & (planetsInitial$"StellarTemp" < 7500) ~ "F",
  (planetsInitial$"StellarTemp" >= 7500) & (planetsInitial$"StellarTemp" < 10000) ~ "A",
  (planetsInitial$"StellarTemp" >= 10000) & (planetsInitial$"StellarTemp" < 30000) ~ "B",
  (planetsInitial$"StellarTemp" >= 30000) ~ "O",
  (is.na(planetsInitial$"StellarTemp") & (!is.na(planetsInitial$"SpectralType"))) ~ substr(planetsInitial$"SpectralType", 1, 1),
  (!is.na(planetsInitial$"SpectralType")) ~ substr(planetsInitial$"SpectralType", 1, 1),
  .default = "Other")

planetsInitial <- planetsInitial[!(planetsInitial$SpectralSubType == "Other"), ]

#Adding Stellar Classifications
planetsInitial <- planetsInitial %>% mutate("StellarClass" = case_when(
  (planetsInitial$"SpectralSubType" == "M") & (planetsInitial$"Luminosity" < -1.0969) ~ "Main Sequence",
  (planetsInitial$"SpectralSubType" == "K") & (planetsInitial$"Luminosity" < -0.2218)~ "Main Sequence",
  (planetsInitial$"SpectralSubType" == "G") & (planetsInitial$"Luminosity" < 0.1761) ~ "Main Sequence",
  (planetsInitial$"SpectralSubType" == "F") & (planetsInitial$"Luminosity" < 0.699) ~ "Main Sequence",
  (planetsInitial$"SpectralSubType" == "A") & (planetsInitial$"Luminosity" < 1.3979) ~ "Main Sequence",
  (planetsInitial$"SpectralSubType" == "B") & (planetsInitial$"Luminosity" < 4.477) ~ "Main Sequence",
  (planetsInitial$"SpectralSubType" == "O") ~ "Main Sequence",
  .default = "Gas Giant"))

#Removes planets in systems not in the Main Sequence
planetsInitial <- planetsInitial[(planetsInitial$StellarClass == "Main Sequence"), ]

data<-transactions(data.frame(TID=planetsInitial$HostName, items = planetsInitial$PlanetTempSubType), format = "long")
summary(data)

association.rules <- apriori(data, parameter = list(supp=0.0004, conf=0.5, minlen = 2))
inspect(association.rules)

plot(association.rules, method = "graph", engine = "html")

write(association.rules, "/Users/vgatne/Documents/PIE/Rules/planetAssoc_mainSeq_tempSubTypes-rules.csv", row.names = FALSE, sep = ",")
