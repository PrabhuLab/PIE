#Planet Sub-Type Association

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

#Adding Planet Sub-Types
planetsInitial <- planetsInitial %>% mutate("PlanetSubType" = case_when(
  (planetsInitial$"OrbitalPeriod" < 10) ~ paste("Short", planetsInitial$"PlanetType", sep=" "),
  (planetsInitial$"OrbitalPeriod" >= 10) & (planetsInitial$"OrbitalPeriod" < 100) ~ paste("Medium-Short", planetsInitial$"PlanetType", sep=" "),
  (planetsInitial$"OrbitalPeriod" >= 100) & (planetsInitial$"OrbitalPeriod" < 1000) ~ paste("Medium-Long", planetsInitial$"PlanetType", sep=" "),
  (planetsInitial$"OrbitalPeriod" >= 1000) ~ paste("Long", planetsInitial$"PlanetType", sep=" "),
  .default = "Other"))

planetsInitial <- planetsInitial[!(planetsInitial$PlanetSubType == "Other"), ]

data<-transactions(data.frame(TID=planetsInitial$HostName, items = planetsInitial$PlanetSubType), format = "long")
summary(data)

association.rules <- apriori(data, parameter = list(supp=0.0003, conf=0.5, minlen = 2))
inspect(association.rules)

plot(association.rules, method = "graph", engine = "html")

write(association.rules, "/Users/vgatne/Documents/PIE/Rules/planetAssoc_subTypes-rules.csv", row.names = FALSE, sep = ",")
