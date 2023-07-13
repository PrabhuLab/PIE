#Combined Planet Type and Stellar Type Association with Main Sequence Stars

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
  ((planetsInitial$"PlanetRadius" >= 9.4) | (planetsInitial$"PlanetMass" > 1)) & (planetsInitial$"OrbitalPeriod" < 10) ~ "Hot Jupiter",
  (planetsInitial$"PlanetRadius" >= 9.4) | (planetsInitial$"PlanetMass" > 1) ~ "Gas Giant",
  .default = "Other"))

planetsInitial <- planetsInitial[!(planetsInitial$PlanetType == "Other"), ]

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

planetData <- ddply(planetsInitial, c("HostName"),
                    function(df1)paste(df1$PlanetType,
                                       collapse = ","))

stellarData <- ddply(planetsInitial, c("HostName"),
                     function(df1)paste(df1$SpectralSubType,
                                        collapse = ","))

combined <- function(x, y) { paste(x, y, sep="-", collapse=",") } 
planetStellarData <- data.frame(items = mapply(combined, strsplit(stellarData$V1, ","), strsplit(planetData$V1, ",")))

write.csv(planetStellarData,"/Users/vgatne/Documents/PIE/Data/planetStellarData.csv", quote = FALSE, row.names = FALSE)
data <- read.transactions("/Users/vgatne/Documents/PIE/Data/planetStellarData.csv", format = "basket", sep = ",")

association.rules <- apriori(data, parameter = list(supp=0.0004, conf=0.5, minlen = 2))
inspect(association.rules)

plot(association.rules, method = "graph", engine = "html")
