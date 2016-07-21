# Plot.ly Data Visualizations

install.packages("plotly")
library(plotly)

library(readr)
water_consumed <- read_csv("waterConsumption.csv")
View(water_consumed)
GallonsOz <- (water_consumed$LitersKg / 35.2739619) / 3.7854118
GallonsOz <- round(GallonsOz)
water_consumed <- cbind(water_consumed, GallonsOz)

library(plyr)
water_consumed <- rename(water_consumed, c("Ingredient"="Food"))

resources_consumed <- read_csv("envResources.csv")
View(resources_consumed)

# Convert from lbs. to oz.
resources_consumed$CO2 <- resources_consumed$CO2 / 16

# Cast Grain values as numeric and convert from lbs. to oz.
resources_consumed$Grain <- as.numeric(resources_consumed$Grain)
resources_consumed$Grain <- resources_consumed$Grain / 16

# Remove water column
resources_consumed <- resources_consumed[,-c(3)]

# Cast Land values as numeric and convert from lbs. to oz.
resources_consumed$Land <- as.numeric(resources_consumed$Land)
resources_consumed$Land <- resources_consumed$Land / 16

# Round decimal values to 3 decimal points
# CO2
is.num <- sapply(resources_consumed$CO2, is.numeric)
resources_consumed$CO2[is.num] <- lapply(resources_consumed$CO2[is.num], round, 3)
# Grain
is.num <- sapply(resources_consumed$Grain, is.numeric)
resources_consumed$Grain[is.num] <- lapply(resources_consumed$Grain[is.num], round, 3)
# Land
is.num <- sapply(resources_consumed$Land, is.numeric)
resources_consumed$Land[is.num] <- lapply(resources_consumed$Land[is.num], round, 4)

resources_consumed <- rename(resources_consumed, c("Meat Type"="Food"))
resources_consumed[resources_consumed == "Poultry"] = "Chicken"

library(dplyr)
test <- merge(water_consumed, resources_consumed, by = intersect(names(water_consumed$Food), names(resources_consumed$Food)))
test <- cbind(resources_consumed, water_consumed[, "Food"][match(rownames(resources_consumed), rownames(water_consumed))])
View(test)
