## calculates log of the marginal likelihood using log-sum-exp trick
## function to perform log-sum-exp trick
log_sum_exp <- function(lx) {
  
  ## extract maximum of logged values
  mX <- max(lx)
  
  ## return answer
  out <- mX + log(sum(exp(lx - mX)))
  out
}

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
  
  for (variable in ncol(dataset):1) {
    iter_values_mean[[variable]] <- matrix(0, imputations, maxit)
    iter_values_sd[[variable]] <- matrix(0, imputations, maxit)
  }
  
  output$original <- dataset        #record original dataset
  
  #position of missing data
  data.missing <- is.na(dataset)
  data.present <- !is.na(dataset)
  
  # replace values with observed values
  start.model <- Mclust(dataset[complete.cases(dataset),], G = G)   #initial model
  missing.values <- sim(start.model$modelName, start.model$parameters, nrow(dataset))    #simulate values, same size of original
  for (variable in 1:ncol(dataset)) {
    missing <- which(data.missing[,variable])    #where are the missing values per variable
    dataset[missing,variable] <- missing.values[missing,variable+1]   #place simulated in empty space
  }
  # # function for any number of variables to fill with mean
  # for (variable in 1:ncol(dataset)) {
  #   dataset[,variable] = ifelse(is.na(dataset[,variable]),
  #                                 ave(dataset[,variable], FUN = function(x) mean(x, na.rm = TRUE)),
  #                                 dataset[,variable])
  # }
  
  for (cycle in 1: imputations) {
    
    # a matrix of columns = iterations, rows = variables
    mean <- matrix(0,ncol(dataset),maxit)
    sd <- matrix(0,ncol(dataset),maxit)
    
    current.imputing <- dataset      #dataset being used for this cycle
    
    for (iteration in 1:maxit) {    #reuse of the same dataset
      
      for (variable in 1:ncol(dataset)) {
        
        ######################
        ##########################
        # Problem: currently this is running the model on the original simulated dataset instead of the current imputed dataset.
        #             when running current.imputing, the model does not converge
        #             MAYBE BREAK FOR LOOP EARLY, AND GIVE WARNING
        ##########################
        ######################
        
        model.mclust <- Mclust(current.imputing, G = G)        #run model for that variable
        
        value <- cbind(Parameter = model.mclust$G, Variable = variable, Iteration = iteration, Imputation = cycle)
        parameter <- rbind(parameter, value)      # save the number of components used in Mclust
        
        # collect sigma and mu from mclust
        sigma <- model.mclust$parameters$variance$sigma
        mu <- model.mclust$parameters$mean
        #####################     what are these? are they ever close to zero??
        pro <- model.mclust$parameters$pro
        
        # positions of missing values
        position <- which(data.missing[, variable])
        
        ## marginal for known
        marg_mean <- mu[-variable, , drop = FALSE]
        marg_sigma <- sigma[-variable, -variable, , drop = FALSE]
        
        for (row in position) {
          
          #testing
          print(paste(iteration, variable, row))
          #browser statement
          
          
          ## log-marginal for each component
          lmarg <- map_dbl(1:length(pro), function(i, x, pro, mu, sigma) {
            mclust::dmvnorm(x, mu[, i], sigma[, , i], log = TRUE) + log(pro[i])
          }, x = current.imputing[row, -variable], pro = pro, mu = marg_mean, sigma = marg_sigma)
          ## log-marginal
          lmarg <- log_sum_exp(lmarg)
          
          ## conditional for x2 for all components
          z_given_x_l <- map_dbl(1:length(pro), function(i, x, pro, mu, sigma, denom) {
            mclust::dmvnorm(x, mu[, i], sigma[, , i], log = TRUE) + log(pro[i]) - denom
          }, x = current.imputing[row, -variable], pro = pro, mu = marg_mean, sigma = marg_sigma, denom = lmarg)
          z_given_x <- exp(z_given_x_l)
          stopifnot(all.equal(sum(z_given_x), 1))
          
          draw <- which.is.max(rmultinom(n=1, size=1, prob=z_given_x))       #parameter draw from multinomial
          # conditional mean/variance

          test <- condMVN(mean=mu[,draw],
                          sigma=sigma[,,draw],
                          dependent.ind=variable,
                          given.ind=(1:ncol(dataset))[-variable],
                          X.given=as.numeric(dataset[row, -variable]),
                          check.sigma = TRUE)
          current.imputing[row, variable] <- rnorm(1,test$condMean, test$condVar)        #replace values with conditional draw
        
          }
      }
      
      
      for (variable in ncol(dataset):1) {        #save mean and sd per variable per iteration
        mean[variable, iteration] <- mean(current.imputing[,variable])
        sd[variable, iteration] <- sd(current.imputing[,variable])
      }
    }
    
    for (variable in ncol(dataset):1) {     #combine all means and sd from all cycles
      iter_values_mean[[variable]][cycle,] <- mean[variable,]
      iter_values_sd[[variable]][cycle,] <- sd[variable,]
    }
    
    #output$dataset[[cycle]] <- current.imputing
    
    for (variable in ncol(dataset):1) {       #collect only the final imputed values
      values <- current.imputing[,variable]
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
  output$mclust <- model.mclust
  #End of Function
  return(output)
}