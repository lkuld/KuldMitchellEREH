# Title: RegressionPlots.R
# Objective: Creates regression plots for paper

rm(list=ls())

# Setting up libraries
library(stringr)
library(ggplot2)
library(mgcv)

source("./Code/RegressionPlotFunctions.R")

# Importing data
load("./Data/EREH_data.RData")

########################################################################
#### age cohort plots
########################################################################

# create data on for prediction, ie age from 0 to 80 for the three birth cohorts, number of writers constant at median value

ageSpan <- 0:80 # for estimate and prediction / can be changed individually for estimation below
pred_data <- data.frame(age = rep(ageSpan,3),
                        total_writers_year = median(reg_data$total_writers_year),
                        birth_cohort = rep(levels(reg_data$birth_cohort), each = length(ageSpan))
)

# Apply AgeCohort() function from RegressionPlotFunctions.R 

# Values for depVar can be taken from names(reg_data), ie all variables starting with loc_, dist_ or other_
# dist should be binomial, poisson, or gaussian as appropriate.
# Population offsets can be used in extraVars to reproduce the linear model plots in the paper's appendix. 
# For this, the yearlyPopShare data needs to be merged with the regression data using the year variable. 
# Population shares can then be added as offsets in extraVars, eg "offset(pop_share_uni)"
# To save time, we iterated all plots in a parrelised loop. Happy to share this code upon request.

plotData <- ageCohort(data=reg_data, 
                      dataCond= reg_data$year%in%1700:1932 & # change condition for data selection
                        reg_data$age %in% ageSpan &
                        reg_data$german_1910_borders,
                      depVar="loc_capital", # change the dependent variable
                      dist="binomial", # change to poisson, gaussian, binomial, ...
                      pred_data=pred_data, 
                      model="gam", # change to glm or gam
                      B=2000) # set number of repetitions for confidence interval estimation, 
                              # set to low number for speed, CI will be off but point estimate not affected

ggplot(plotData, aes(age, y_hat)) +
  theme_bw() +
  geom_line(aes(linetype=birth_cohort, color=birth_cohort),size=1.5)+
  scale_color_viridis_d(direction=-1) +
  geom_errorbar(aes(ymin=lwr, ymax= upr,color=birth_cohort),size=.5,linetype="dotted") +
  labs(y="capital", linetype="Born", color="Born") +
  theme(legend.position = "bottom")

########################################################################
#### year/time plots
########################################################################

# new data for modelling time effect: 30 year old writer, median number of yearly writers
pred_data <- data.frame(age = 30,
                        year=1750:1932,
                        total_writers_year = median(reg_data$total_writers_year)
)

# use the byYear function from RegressionPlotFunctions.R with the same arguments as above

plotData <- byYear(data=reg_data, 
                   dataCond= reg_data$year%in%1700:1932 & 
                     reg_data$age %in% 18:65 &
                     reg_data$german_1910_borders,
                   depVar="loc_capital", 
                   dist="binomial", 
                   pred_data=pred_data, 
                   model="gam",
                   B=2000)

ggplot(plotData, aes(year, y_hat)) +
  geom_errorbar(aes(ymin=lwr, ymax= upr),color="grey60",size=.7) +
  geom_line(size=1.5)+
  labs(y="capital") +
  theme_bw() 

