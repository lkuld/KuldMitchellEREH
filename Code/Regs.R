# Title: Regressions.R
# Objective: REGRESSIONS FOR PAPER

rm(list=ls())

library(stringr)
library(lmtest)
library(sandwich)

# Importing data
load("./Data/EREH_data.RData")

reg_data$under18 <- reg_data$age < 18
reg_data$over40 <- reg_data$age >40

cond <- reg_data$year%in%1700:1932 & reg_data$german_1910_borders

depVars <- names(reg_data)[str_detect(names(reg_data),"^loc")]

regList <- sapply(depVars, function(x) NULL)
seList <- sapply(depVars, function(x) NULL)
pList <- sapply(depVars, function(x) NULL)

for(i in seq_along(depVars)){

formu <- as.formula(paste(depVars[i], "~ under18*birth_cohort + over40*birth_cohort + log1p(total_writers_year)"))
regList[[depVars[i]]] <- glm(formu   , reg_data[cond,], family = binomial())
seList[[depVars[i]]] <- coeftest(regList[[depVars[i]]], vcov = vcovCL, Cluster = ~author)[,2]
pList[[depVars[i]]] <- coeftest(regList[[depVars[i]]], vcov = vcovCL, Cluster = ~author)[,4]
}

#texreg::screenreg(regList, override.se = seList, override.pvalues = pList)

### proximity

cond <- reg_data$year%in%1700:1932 & reg_data$german_1910_borders

depVars <- c("dist_1plus_30km", "dist_2plus_10km", "dist_nearest_writer", "dist_num_writers_10km") 
modVars <- list(binomial, binomial(), poisson(), poisson()) 

regList <- sapply(depVars, function(x) NULL)
seList <- sapply(depVars, function(x) NULL)
pList <- sapply(depVars, function(x) NULL)

for(i in seq_along(depVars)){
  
  formu <- as.formula(paste(depVars[i], "~ under18*birth_cohort + over40*birth_cohort + log1p(total_writers_year)"))
  regList[[depVars[i]]] <- glm(formu   , reg_data[cond,],family = modVars[[i]])
  seList[[depVars[i]]] <- coeftest(regList[[depVars[i]]], vcov = vcovCL, Cluster = ~author)[,2]
  pList[[depVars[i]]] <- coeftest(regList[[depVars[i]]], vcov = vcovCL, Cluster = ~author)[,4]
}

#texreg::screenreg(regList, override.se = seList, override.pvalues = pList)


### moves & publications

cond <- reg_data$year%in%1700:1932 & reg_data$german_1910_borders

depVars <- c("other_move", "other_distance_location_last_year", "other_distance_birth_location", "other_published_in_year") 
modVars <- list(binomial, poisson(), poisson(), binomial()) 

regList <- sapply(depVars, function(x) NULL)
seList <- sapply(depVars, function(x) NULL)
pList <- sapply(depVars, function(x) NULL)

for(i in seq_along(depVars)){
  
  formu <- as.formula(paste(depVars[i], "~ under18*birth_cohort + over40*birth_cohort + log1p(total_writers_year)"))
  regList[[depVars[i]]] <- glm(formu   , reg_data[cond,],family = modVars[[i]])
  seList[[depVars[i]]] <- coeftest(regList[[depVars[i]]], vcov = vcovCL, Cluster = ~author)[,2]
  pList[[depVars[i]]] <- coeftest(regList[[depVars[i]]], vcov = vcovCL, Cluster = ~author)[,4]
}

#texreg::screenreg(regList, override.se = seList, override.pvalues = pList)


