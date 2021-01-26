## calculates log of the marginal likelihood using log-sum-exp trick
## function to perform log-sum-exp trick
log_sum_exp <- function(lx) {
  
  ## extract maximum of logged values
  mX <- max(lx)
  
  ## return answer
  out <- mX + log(sum(exp(lx - mX)))
  out
}

mclust_version2 <- function(dataset, imputations = 10, maxit = 5, G = 1:9, diagnosis_plots = FALSE, seed = NULL) {
  require(GGally)
  require(mclust)
  require(condMVNorm)
  require(tidyverse)
  require(nnet)
  require(MASS)
  # setting seed
  if (is.null(seed)) {
    seed <- sample(1:999999, 1)
    set.seed(seed)
  } else {
    set.seed(seed)
  }
  #outcome of function
  output <- NULL
  parameter <- NULL
  imp <- NULL
  iter_values_mean <- NULL
  iter_values_sd <- NULL
  
  for (variable in ncol(dataset):1) {
    iter_values_mean[[variable]] <- matrix(NA, imputations, maxit)
    iter_values_sd[[variable]] <- matrix(NA, imputations, maxit)
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
  
  #directory for plots
  if (diagnosis_plots == TRUE) {
    dir.create(as.character(seed))
  }
  
  
  for (cycle in 1: imputations) {
    
    # a matrix of columns = iterations, rows = variables
    mean <- matrix(NA,ncol(dataset),maxit)
    sd <- matrix(NA,ncol(dataset),maxit)
    
    current.imputing <- dataset      #dataset being used for this cycle
    
    #some diagnosis plots through each iteration
    if (diagnosis_plots == TRUE) {
      title <- paste("iteration_",cycle,"_0.png", sep = "")
      ggsave(path = as.character(seed), filename = title, plot = ggpairs(current.imputing), height = 20, width = 27, units = "cm")
    } 
    
    tryCatch({
      
      for (iteration in 1:maxit) {
        
        model.mclust <- Mclust(current.imputing, G = G)        #run model for that iteration
        
        
        
        
        value <- cbind(Parameter = model.mclust$G, Iteration = iteration, Imputation = cycle)
        parameter <- rbind(parameter, value)      # save the number of components used in Mclust
        
        # collect sigma and mu from mclust
        sigma <- model.mclust$parameters$variance$sigma
        mu <- model.mclust$parameters$mean
        pro <- model.mclust$parameters$pro
        
        # rows with missing values
        x <- rownames(output$original[!complete.cases(output$original), ])
        
        for (row in x) {
          
          # positions of missing values
          variable <- which(data.missing[row,])
          
          #check if all variables are missing or not(conditional draw)
          if (length(variable) != ncol(dataset)) {
            
            ## marginal for known
            marg_mean <- mu[-variable, , drop = FALSE]
            marg_sigma <- sigma[-variable, -variable, , drop = FALSE]
            
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
            
          } else{
            
            z_given_x <- pro               #if all variable on row are missing, draw randomly
            
          }
          
          
          draw <- which.is.max(rmultinom(n=1, size=1, prob=z_given_x))       #parameter draw from multinomial
          
          # conditional mean/variance
          
          test <- condMVN(mean=mu[,draw],
                          sigma=sigma[,,draw],
                          dependent.ind=variable,
                          given.ind=(1:ncol(dataset))[-variable],
                          X.given=as.numeric(dataset[row, -variable]),
                          check.sigma = FALSE)
          current.imputing[row, variable] <- mvrnorm(1, test$condMean,test$condVar)        #replace values with conditional draw
          #print(mvrnorm(1, test$condMean,test$condVar))
         ################### check what comes out when two values are being imputed
        }
        
        for (variable in ncol(dataset):1) {        #save mean and sd per variable per iteration
          mean[variable, iteration] <- mean(current.imputing[,variable])
          sd[variable, iteration] <- sd(current.imputing[,variable])
        }
        
        #some diagnosis plots through each iteration
        if (diagnosis_plots == TRUE) {
          title <- paste("iteration_",cycle,"_",iteration,".png", sep = "")
          ggsave(path = as.character(seed), filename = title, plot = ggpairs(current.imputing), height = 20, width = 27, units = "cm")
        }
        
        print(paste("Cycle ", cycle, ", iteration ", iteration, " complete!", sep = ""))        
        
      }
      
    }, error = function(e){print(paste("Cycle ", cycle, " failed to converge!"))})
    
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
  output$seed <- seed
  #End of Function
  return(output)
}