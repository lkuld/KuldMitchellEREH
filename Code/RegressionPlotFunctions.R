# Title: RegressionPlots.R
# Objective: Functions to be used in RegressionPlots.R

## A:  cohorts

# function for effects over age for three cohorts
# repeats estimation B times to cluster bootstrap the distribution of estimates

ageCohort <- function(data, dataCond=TRUE, ageSpan=0:80, depVar, model="glm", dist, pred_data, extraVars=NA, B=2000){
  
  df <- data[dataCond,] # subset data
  
  authors <- unique(df$author) # create a vector of authors
  
  set.seed(1) # set random seed
  
  # if we want to add further covariates
  if(is.na(extraVars)) extraVars <- NULL
  if(!is.null(extraVars)) extraVars <- paste("+", paste(extraVars, collapse = "+"))
  
  # specify regression equation dependent on empirical model, see technical appendix for a discussion
  if(model=="glm")  formu <- as.formula(paste(depVar," ~ poly(age,5)*birth_cohort", extraVars))
  
  if(model=="gam")  extraVars <- stringr::str_replace(extraVars,"log1p|log", "s")
  if(model=="gam")  formu <- as.formula(paste(depVar," ~   s(age) + birth_cohort + s(age, by= birth_cohort)", extraVars))
  
# estimation, repeated B times
  pred <- list()
  for (i in 1:B) {cat(i,", ")
    boot <- sample(authors, length(authors), replace = TRUE)
    boot_data <- do.call(rbind, lapply(boot, function(x) df[df$author%in%x ,]  ))
    
    if(model=="glm") fit <- glm( formu,  family = dist, data = boot_data)
    if(model=="gam") fit <- gam( formu,  family = dist, data = boot_data)
    
    pred[[i]] <- predict(fit, newdata = pred_data,  type = "response")
  }
  
  # take 0.025 and 0.975 quantiles of estimates for confidence interval estimation
  pred_data$lwr <- sapply(1:nrow(pred_data), function(k) quantile(sapply(pred, function(x) x[k]), 0.025))
  pred_data$upr <- sapply(1:nrow(pred_data), function(k) quantile(sapply(pred, function(x) x[k]), 0.975))

  # take whole sample for estimation
  if(model=="glm") fit <- glm( formu,  family = dist, data = df)
  if(model=="gam") fit <- gam( formu,  family = dist, data = df)
  
  pred_data$y_hat <- predict(fit, newdata = pred_data,  type = "response")
  
  return(pred_data)

}

##  B: year 

# similar function for effect over time

byYear <- function(data, dataCond=TRUE, depVar, model="glm", dist, pred_data, extraVars=NA, B=2000){
  
  df <- data[dataCond,]
  
  # create a vector of authors
  authors <- unique(df$author)
  
  set.seed(1)
  
  if(is.na(extraVars)) extraVars <- NULL
  pred <- list()
  if(!is.null(extraVars)) extraVars <- paste("+", paste(extraVars, collapse = "+"))
  
  if(model=="glm")  formu <- as.formula(paste(depVar," ~ poly(year,5) + poly(age,5)", extraVars))
  
  if(model=="gam") extraVars <- str_replace(extraVars,"log1p|log", "s")
  if(model=="gam")  formu <- as.formula(paste(depVar," ~   s(year)  + s(age)", extraVars))
  
  pred <- list()
  for (i in 1:B) {cat(i,", ")
    boot <- sample(authors, length(authors), replace = TRUE)
    boot_data <- do.call(rbind, lapply(boot, function(x) df[df$author%in%x,]  ))
    
    if(model=="glm") fit <- glm( formu,  family = dist, data = boot_data)
    if(model=="gam") fit <- gam( formu,  family = dist, data = boot_data)
    
    pred[[i]] <- predict(fit, newdata = pred_data,  type = "response")
  }
  
  pred_data$lwr <- sapply(1:nrow(pred_data), function(k) quantile(sapply(pred, function(x) x[k]), 0.025))
  pred_data$upr <- sapply(1:nrow(pred_data), function(k) quantile(sapply(pred, function(x) x[k]), 0.975))
  
  if(model=="glm") fit <- glm( formu,  family = dist, data = df)
  if(model=="gam") fit <- gam( formu,  family = dist, data = df)
  
  pred_data$y_hat <- predict(fit, newdata = pred_data,
                             type = "response")
  
  return(pred_data)
}
