mclust_version1 <- function(dataset, imputations = 10, maxit = 5, G = 1:9) {
  require(mclust)
  require(condMVNorm)
  require(tidyverse)
  require(nnet)
  #outcome of function
  output <- NULL
  parameter <- NULL
  imp <- NULL
  iter_values_mean <- NULL
  iter_values_sd <- NULL
  
  output$original <- dataset        #record original dataset
  
  #position of missing data
  data.missing <- is.na(dataset)
  data.present <- !is.na(dataset)
  
  # replace values with observed values
  start.model <- Mclust(dataset[complete.cases(dataset),], G = G)   #initial model
  missing.values <- sim(start.model$modelName, start.model$parameters, nrow(dataset))    #simulate values, same size of original
  for (variable in 1:ncol(dataset)) {
    missing <- which(data.missing[,variable])    #where are the missing values per variable
    
    for (row in missing) {
      dataset[row,variable] <- missing.values[row,variable+1]   #place simulated in empty space
    }
  }
  # # function for any number of variables to fill with mean
  # for (variable in 1:ncol(dataset)) {
  #   dataset[,variable] = ifelse(is.na(dataset[,variable]),
  #                                 ave(dataset[,variable], FUN = function(x) mean(x, na.rm = TRUE)),
  #                                 dataset[,variable])
  # }
  
  for (cycle in 1: imputations) {
    
    mean <- NULL
    sd <- NULL
    
    imputing <- dataset      #dataset being used for this cycle
    
    for (iteration in 1:maxit) {    #reuse of the same dataset
      
      for (variable in 1:ncol(dataset)) {
        model.mclust <- Mclust(dataset, G = G)        #run model for that variable
        
        value <- cbind(Parameter = model.mclust$G, Variable = variable, Iteration = iteration, Imputation = cycle)
        parameter <- rbind(parameter, value)      # save the number of components used in Mclust
        # collect sigma and mu from mclust
        sigma <- model.mclust$parameters$variance$sigma
        mu <- model.mclust$parameters$mean
        
        position <- which(data.missing[,variable])       # position of missing in the variable
        
        for (row in position) {
          draw <- which.is.max(rmultinom(n=1, size=1, prob=model.mclust$parameters$pro))       #parameter draw from multinomial
          # conditional mean/variance 
          given <- which(data.present[row,])
          test <- condMVN(mean=mu[,draw],
                          sigma=sigma[,,draw],
                          dependent=variable,
                          given=which(data.present[row,]),
                          X.given=as.numeric(dataset[row,given]),
                          check.sigma = FALSE)
          imputing[row,variable] <- rnorm(1,test$condMean, test$condVar)        #replace values with conditional draw
        }
      }
      
      for (variable in ncol(dataset):1) {        #save mean and sd per variable per iteration
        mean[[variable]] <- cbind(mean[[variable]], mean(imputing[,variable]))
        sd[[variable]] <- cbind(sd[[variable]], sd(imputing[,variable]))
      }
    }
    
    for (variable in ncol(dataset):1) {     #combine all means and sd from all cycles
      iter_values_mean[[variable]] <- rbind(iter_values_mean[[variable]], mean[[variable]])
      iter_values_sd[[variable]] <- rbind(iter_values_sd[[variable]], sd[[variable]])
    }
    
    output$dataset[[cycle]] <- imputing
    
    for (variable in ncol(dataset):1) {       #collect only the final imputed values
      values <- imputing[,variable]
      values[!data.missing] <- NA
      imp[[variable]] <- cbind(imp[[variable]], values[complete.cases(values)])
    }
  }
  
  for (variable  in 1:ncol(dataset)) {           #allow the "imp" to be understood better with actual name
    colnames(imp[[variable]]) <- rep(colnames(dataset)[variable], imputations)
    colnames(iter_values_mean[[variable]]) <- rep(colnames(dataset)[variable], maxit)
    colnames(iter_values_sd[[variable]]) <- rep(colnames(dataset)[variable], maxit)
  }
  # combine all output elements
  output$iteration <- maxit
  output$imputation <- imputations
  output$G <- G
  output$imp <- imp
  output$parameter <- as.data.frame(parameter)
  output$missing <- data.missing
  output$iter_mean <- iter_values_mean
  output$iter_sd <- iter_values_sd
  #End of Function
  return(output)
}