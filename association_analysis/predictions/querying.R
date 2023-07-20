#Querying Data with Significant Rules

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

#Adding Spectral Sub-Type and adjusting for consistency and NAs in data
planetsInitial <- planetsInitial %>% mutate("SpectralSubType" = case_when(
  (planetsInitial$"StellarTemp" >= 2400) & (planetsInitial$"StellarTemp" < 3700) ~ "M",
  (planetsInitial$"StellarTemp" >= 3700) & (planetsInitial$"StellarTemp" < 5200) ~ "K",
  (planetsInitial$"StellarTemp" >= 5200) & (planetsInitial$"StellarTemp" < 6000) ~ "G",
  (planetsInitial$"StellarTemp" >= 6000) & (planetsInitial$"StellarTemp" < 7500) ~ "F",
  (planetsInitial$"StellarTemp" >= 7500) & (planetsInitial$"StellarTemp" < 10000) ~ "A",
  (planetsInitial$"StellarTemp" >= 10000) & (planetsInitial$"StellarTemp" < 30000) ~ "B",
  (planetsInitial$"StellarTemp" >= 30000) ~ "O",
  (is.na(planetsInitial$"StellarTemp") & (!is.na(planetsInitial$"SpectralType"))) ~ substr(planetsInitial$"SpectralType", 1, 1),
  (!is.na(planetsInitial$"SpectralType")) ~ substr(planetsInitial$"SpectralType", 1, 1),
  .default = "Other"))

planetsInitial <- planetsInitial[!(planetsInitial$SpectralSubType == "Other"), ]

planetsInitial <- planetsInitial %>% mutate("PlanetSubType" = case_when(
  (planetsInitial$"OrbitalPeriod" < 10) ~ paste(" Short", planetsInitial$"PlanetType", sep=" "),
  (planetsInitial$"OrbitalPeriod" >= 10) & (planetsInitial$"OrbitalPeriod" < 100) ~ paste("Medium-Short", planetsInitial$"PlanetType", sep=" "),
  (planetsInitial$"OrbitalPeriod" >= 100) & (planetsInitial$"OrbitalPeriod" < 1000) ~ paste("Medium-Long", planetsInitial$"PlanetType", sep=" "),
  (planetsInitial$"OrbitalPeriod" >= 1000) ~ paste(" Long", planetsInitial$"PlanetType", sep=" "),
  .default = "Other"))

planetsInitial <- planetsInitial[!(planetsInitial$PlanetSubType == "Other"), ]

planetData <- ddply(planetsInitial, c("HostName"),
                    function(df1)paste(df1$PlanetSubType,
                                       collapse = ","))

stellarData <- ddply(planetsInitial, c("HostName"),
                     function(df1)paste(df1$SpectralSubType,
                                        collapse = ","))

stellarData$V1[nchar(stellarData$V1) > 1] <- substr(stellarData$V1, 1, 1)

orbitalPlanetStellarData <- cbind(stellarData, planetData$V1)

colnames(orbitalPlanetStellarData) = c("Host", "StellarSubType", "PlanetTypes")

#green
results_M_mediumshortSubSaturn.mediumlongSubSaturn <- subset(orbitalPlanetStellarData, 
                                                             (orbitalPlanetStellarData$StellarSubType == "M" & 
                                                               grepl("Medium-Short Sub-Saturn", orbitalPlanetStellarData$PlanetTypes, fixed = TRUE) & 
                                                              !grepl("Medium-Long Sub-Saturn", orbitalPlanetStellarData$PlanetTypes, fixed = TRUE)))

write.csv(results_M_mediumshortSubSaturn.mediumlongSubSaturn, "/Users/vgatne/Documents/PIE/Predictions/results_M_mediumshortSubSaturn.mediumlongSubSaturn.csv")

results_G_shortMiniNeptune_mediumlongGasGiant.longGasGiant <- subset(orbitalPlanetStellarData, 
                                                             (orbitalPlanetStellarData$StellarSubType == "G" & 
                                                              grepl(" Short Mini-Neptune", orbitalPlanetStellarData$PlanetTypes, fixed = TRUE) & 
                                                              grepl("Medium-Long Gas Giant", orbitalPlanetStellarData$PlanetTypes, fixed = TRUE) &
                                                              !grepl(" Long Gas Giant", orbitalPlanetStellarData$PlanetTypes, fixed = TRUE)))

write.csv(results_G_shortMiniNeptune_mediumlongGasGiant.longGasGiant, "/Users/vgatne/Documents/PIE/Predictions/results_G_shortMiniNeptune_mediumlongGasGiant.longGasGiant.csv")

results_M_mediumlongGasGiant.longGasGiant <- subset(orbitalPlanetStellarData, 
                                                             (orbitalPlanetStellarData$StellarSubType == "M" & 
                                                              grepl("Medium-Long Gas Giant", orbitalPlanetStellarData$PlanetTypes, fixed = TRUE) & 
                                                              !grepl(" Long Gas Giant", orbitalPlanetStellarData$PlanetTypes, fixed = TRUE)))

write.csv(results_M_mediumlongGasGiant.longGasGiant, "/Users/vgatne/Documents/PIE/Predictions/results_M_mediumlongGasGiant.longGasGiant.csv")

results_M_shortMiniNeptune_mediumlongMiniNeptune.longGasGiant <- subset(orbitalPlanetStellarData, 
                                                              (orbitalPlanetStellarData$StellarSubType == "M" & 
                                                               grepl(" Short Mini-Neptune", orbitalPlanetStellarData$PlanetTypes, fixed = TRUE) & 
                                                               grepl("Medium-Long Mini-Neptune", orbitalPlanetStellarData$PlanetTypes, fixed = TRUE) &
                                                               !grepl(" Long Gas Giant", orbitalPlanetStellarData$PlanetTypes, fixed = TRUE)))

write.csv(results_M_shortMiniNeptune_mediumlongMiniNeptune.longGasGiant, "/Users/vgatne/Documents/PIE/Predictions/results_M_shortMiniNeptune_mediumlongMiniNeptune.longGasGiant.csv")

#yellow
results_M_longGasGiant_shortMiniNeptune.mediumlongMiniNeptune <- subset(orbitalPlanetStellarData, 
                                                              (orbitalPlanetStellarData$StellarSubType == "M" & 
                                                               grepl(" Long Gas Giant", orbitalPlanetStellarData$PlanetTypes, fixed = TRUE) & 
                                                               grepl(" Short Mini-Neptune", orbitalPlanetStellarData$PlanetTypes, fixed = TRUE) &
                                                               !grepl("Medium-Long Mini-Neptune", orbitalPlanetStellarData$PlanetTypes, fixed = TRUE)))

write.csv(results_M_longGasGiant_shortMiniNeptune.mediumlongMiniNeptune, "/Users/vgatne/Documents/PIE/Predictions/results_M_longGasGiant_shortMiniNeptune.mediumlongMiniNeptune.csv")


calculateAxis <- function(stellarMass, period) {(((6.67e-11 * (stellarMass * 1.989e+30) * ((period * 86400) ^ 2)) / (4 * (pi ^ 2))) ^ (1 / 3)) / 1.496e+11}

calculateTemp <- function(luminosity, axis) {(((10 ^ luminosity) * 3.827e+26) / (16 * pi * 5.67e-08 * ((axis * 1.496e+11) ^ 2))) ^ (1 / 4)}

planetsInitial$"SemiMajorAxis"[is.na(planetsInitial$"SemiMajorAxis")] <- calculateAxis(planetsInitial$"StellarMass", planetsInitial$"OrbitalPeriod")

planetsInitial$"PlanetTemp"[is.na(planetsInitial$"PlanetTemp")] <- calculateTemp(planetsInitial$"Luminosity", planetsInitial$"SemiMajorAxis")

planetsInitial$PlanetTemp[which(is.na(planetsInitial$PlanetTemp))] <- calculateTemp(planetsInitial$Luminosity, planetsInitial$SemiMajorAxis)

planetsInitial <- planetsInitial %>% mutate("PlanetTempSubType" = case_when(
  (planetsInitial$"PlanetTemp" >= 1000) ~ paste("Hot", planetsInitial$"PlanetType", sep=" "),
  (planetsInitial$"PlanetTemp" >= 400) & (planetsInitial$"PlanetTemp" < 1000) ~ paste("Warm", planetsInitial$"PlanetType", sep=" "),
  (planetsInitial$"PlanetTemp" >= 200) & (planetsInitial$"PlanetTemp" < 400) ~ paste("Temperate", planetsInitial$"PlanetType", sep=" "),
  (planetsInitial$"PlanetTemp" < 200) ~ paste("Cold", planetsInitial$"PlanetType", sep=" "),
  .default = "Other"))

planetsInitial <- planetsInitial[!(planetsInitial$PlanetTempSubType == "Other"), ]

planetData <- ddply(planetsInitial, c("HostName"),
                    function(df1)paste(df1$PlanetTempSubType,
                                       collapse = ","))

stellarData <- ddply(planetsInitial, c("HostName"),
                     function(df1)paste(df1$SpectralSubType,
                                        collapse = ","))

stellarData$V1[nchar(stellarData$V1) > 1] <- substr(stellarData$V1, 1, 1)

tempPlanetStellarData <- cbind(stellarData, planetData$V1)

colnames(tempPlanetStellarData) = c("Host", "StellarSubType", "PlanetTypes")

#green
results_hotGasGiant_hotMiniNeptune.temperateGasGiant <- subset(tempPlanetStellarData,
                                                              (grepl("Hot Gas Giant", tempPlanetStellarData$PlanetTypes, fixed = TRUE) &
                                                               grepl("Hot Mini-Neptune", tempPlanetStellarData$PlanetTypes, fixed = TRUE) &
                                                               !grepl("Temperate Gas Giant", tempPlanetStellarData$PlanetTypes, fixed = TRUE)))

write.csv(results_hotGasGiant_hotMiniNeptune.temperateGasGiant, "/Users/vgatne/Documents/PIE/Predictions/results_hotGasGiant_hotMiniNeptune.temperateGasGiant.csv")

#yellow
results_M_coldMiniNeptune_warmTerrestrial.temperateMiniNeptune <- subset(tempPlanetStellarData,
                                                              (tempPlanetStellarData$StellarSubType == "M" &
                                                               grepl("Cold Mini-Neptune", tempPlanetStellarData$PlanetTypes, fixed = TRUE) &
                                                               grepl("Warm Terrestrial", tempPlanetStellarData$PlanetTypes, fixed = TRUE) &
                                                               !grepl("Temperate Mini-Neptune", tempPlanetStellarData$PlanetTypes, fixed = TRUE)))

write.csv(results_M_coldMiniNeptune_warmTerrestrial.temperateMiniNeptune, "/Users/vgatne/Documents/PIE/Predictions/results_M_coldMiniNeptune_warmTerrestrial.temperateMiniNeptune.csv")

results_coldTerrestrial_warmTerrestrial.temperateTerrestrial <- subset(tempPlanetStellarData,
                                                               (grepl("Cold Terrestrial", tempPlanetStellarData$PlanetTypes, fixed = TRUE) &
                                                                grepl("Warm Terrestrial", tempPlanetStellarData$PlanetTypes, fixed = TRUE) &
                                                                !grepl("Temperate Terrestrial", tempPlanetStellarData$PlanetTypes, fixed = TRUE)))

write.csv(results_coldTerrestrial_warmTerrestrial.temperateTerrestrial, "/Users/vgatne/Documents/PIE/Predictions/results_coldTerrestrial_warmTerrestrial.temperateTerrestrial.csv")


#PlanetTypes with Hot Jupiters
planetsInitial <- planetsInitial %>% mutate("PlanetType" = case_when(
  (planetsInitial$"PlanetRadius" > 0) & (planetsInitial$"PlanetRadius" < 1.7) ~ "Terrestrial",
  (planetsInitial$"PlanetRadius" >= 1.7) & (planetsInitial$"PlanetRadius" < 3.9) ~ "Mini-Neptune",
  ((planetsInitial$"PlanetRadius" >= 3.9) & (planetsInitial$"PlanetRadius" < 9.4)) & (planetsInitial$"PlanetMass" < 1) ~ "Sub-Saturn",
  ((planetsInitial$"PlanetRadius" >= 9.4) | (planetsInitial$"PlanetMass" > 1)) & (planetsInitial$"OrbitalPeriod" < 10) ~ "Hot Jupiter",
  (planetsInitial$"PlanetRadius" >= 9.4) | (planetsInitial$"PlanetMass" > 1) ~ "Gas Giant",
  .default = "Other"))

planetsInitial <- planetsInitial[!(planetsInitial$PlanetType == "Other"), ]

planetsInitial <- planetsInitial[!(planetsInitial$PlanetTempSubType == "Other"), ]

planetData <- ddply(planetsInitial, c("HostName"),
                    function(df1)paste(df1$PlanetType,
                                       collapse = ","))

stellarData <- ddply(planetsInitial, c("HostName"),
                     function(df1)paste(df1$SpectralSubType,
                                        collapse = ","))

stellarData$V1[nchar(stellarData$V1) > 1] <- substr(stellarData$V1, 1, 1)

allPlanetStellarData <- cbind(stellarData, planetData$V1)

colnames(allPlanetStellarData) = c("Host", "StellarSubType", "PlanetTypes")

#green
results_M_hotJupiter.gasGiant <- subset(allPlanetStellarData, 
                                  (allPlanetStellarData$StellarSubType == "M" & 
                                   grepl("Hot Jupiter", allPlanetStellarData$PlanetTypes, fixed = TRUE) &
                                   !grepl("Gas Giant", allPlanetStellarData$PlanetTypes, fixed = TRUE)))

write.csv(results_M_hotJupiter.gasGiant, "/Users/vgatne/Documents/PIE/Predictions/results_M_hotJupiter.gasGiant.csv")
