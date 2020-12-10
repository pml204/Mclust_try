mclust_version1 <- function(dataset, imputations = 10, maxit = 5, G = 1:9, save_parameter = TRUE) {
  
  require(mclust)
  require(condMVNorm)
  require(tidyverse)
  
  #position of missing data
  data.missing <- is.na(dataset)
  data.present <- !is.na(dataset)
  
  
  
  
  # replace values with observed values
  start.model <- mclustBIC(dataset[complete.cases(dataset),], G = G)
  missing.values <- sim(model.mclust$modelName, model.mclust$parameters, nrow(dataset))
  for (variable in 1:ncol(dataset)) {
    missing <- which(data.missing[,variable])
    
    for (row in missing) {
      dataset[row,variable] <- missing.values[row,variable+1]
    }
    
    
  }
  
  
  
  # # function for any number of variables to fill with mean
  # for (variable in 1:ncol(dataset)) {
  #   dataset[,variable] = ifelse(is.na(dataset[,variable]),
  #                                 ave(dataset[,variable], FUN = function(x) mean(x, na.rm = TRUE)),
  #                                 dataset[,variable])
  # }
  
  
  #outcome of function
  output <- NULL
  parameter <- NULL
  imp <- NULL
  
  
  for (cycle in 1: imputations) {
    
    imputing <- dataset
    
    for (iteration in 1:maxit) {
      
      
      for (variable in 1:ncol(dataset)) {
        
        
        # run best BIC model
        model.mclust <- Mclust(dataset, G = G)
        
        
        #save parameter
        if (save_parameter == TRUE) {
          value <- cbind(Parameter = model.mclust$G, Variable = variable, Iteration = iteration, Imputation = cycle)
          parameter <- rbind(parameter, value)
        }
        
        
        # collect sigma and mu from mclust
        sigma <- model.mclust$parameters$variance$sigma
        mu <- model.mclust$parameters$mean
        
        
        #position of missing data
        position <- which(data.missing[,variable])
        
        for (row in position) {
          
          
          draw <- sample(1:model.mclust$G,1,replace = FALSE,prob = model.mclust$parameters$pro)
          # random draw of sigma
          var <- sigma[,,draw]
          #random draw of mu
          mu.it <- mu[,draw]
          #variable being imputed now
          dependent <- variable
          #position of given variables
          given <- which(data.present[row,])
          #values of given variables
          x.given <- as.numeric(dataset[row,given])
          
          
          # conditional mean/variance 
          test <- condMVN(mean=mu.it, sigma=var, dependent=dependent, given=given,
                          X.given=x.given, check.sigma = FALSE)
          
          imputing[row,variable] <- rnorm(1,test$condMean, test$condVar)
        }
        
      }
      
    }
    
    imputing <- imputing %>%
      mutate(Imputation = cycle)
    
    output$dataset <- rbind(output$dataset, imputing)
    
    
    #value with only missing data
    for (variable in ncol(dataset):1) {
      
      values <- imputing[,variable]
      values[!data.missing] <- NA
      values <- values[complete.cases(values)]
      
      # names <- colnames(dataset)[variable]
      
      imp[[variable]] <- cbind(imp[[variable]], values)
    }
    
    
    
  }
  
  output$iteration <- maxit
  output$imputation <- imputations
  output$imp <- imp
  output$parameter <- as.data.frame(parameter)
  output$missing <- data.missing
  return(output)
  #End of Function
}
