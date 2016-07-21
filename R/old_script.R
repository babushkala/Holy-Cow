# OLD SCRIPT

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

library(dplyr)
test <- merge(water_consumed, resources_consumed, by = intersect(names(water_consumed$Food), names(resources_consumed$Food)))
test <- cbind(resources_consumed, water_consumed[, "Food"][match(rownames(resources_consumed), rownames(water_consumed))])
View(test)
rm(test)