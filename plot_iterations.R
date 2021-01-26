plot_iterations <- function(dataset, print_plot = TRUE, save_plot = FALSE) {
  require(ggplot2)
  require(tidyverse)
  require(reshape2)
  require(ggforce)
  #outcome of function
  grouped_values <- NULL
  plotting.data <- NULL
  
  for (variable in ncol(dataset$original):1) {
    chain_values_mean <- as.data.frame(dataset$iter_mean[[variable]]) 
    chain_values_sd <- as.data.frame(dataset$iter_sd[[variable]])
    for (row in 1:dataset$imputation) {
      chain <- cbind(Mean = t(chain_values_mean[row,]),Sd = t(chain_values_sd[row,]), Chain = row)
      grouped_values[[variable]] <- rbind(grouped_values[[variable]], chain)
    }
    colnames(grouped_values[[variable]])[1:2] <- c("Mean", "Sd")
    grouped_values[[variable]] <- as.data.frame(grouped_values[[variable]]) %>%
      mutate(Iteration = rep(1:dataset$iteration, dataset$imputation),
             Chain = factor(Chain))
    vars <- cbind(melt(grouped_values[[variable]], id.vars = c("Chain","Iteration"), measure.vars = c("Mean","Sd")),Variable = variable)
    plotting.data <- rbind(plotting.data, vars)
  }
  colnames(plotting.data)[3] <- "Method"
  var.labs <- colnames(dataset$original)
  names(var.labs) <- rep(1:(ncol(dataset$original)))
  if (ncol(dataset$original)%%3 == 0){
    #for exactly number of plots multiple of 3
    for (i in seq(1:(ncol(dataset$original)%/%3))) {
      plot <- ggplot(data = plotting.data, aes(x = Iteration , y = value)) +
        geom_line(aes(color = Chain)) +
        facet_wrap_paginate(Variable~Method, scales = "free", ncol = 2, nrow = 3, page = i,
                            labeller = labeller(Variable = var.labs)) +
        ylab(NULL) +
        #theme(legend.position = "none") +
        ggtitle("Mean and Variance through Imputation Chains")
      if (print_plot == TRUE) {
        print(plot)
      }
      if (save_plot == TRUE) {
        title <- paste("iterations",i,".png", sep = "")
        ggsave(title, plot = plot, height = 30, width = 30, units = "cm")
      }
    }
  } else {
    for (i in seq(1:((ncol(dataset$original)%/%3)+1))) {
      if (i == tail(seq(1:((ncol(dataset$original)%/%3)+1)), n=1)) {
        plot <- ggplot(data = plotting.data, aes(x = Iteration , y = value)) +
          geom_line(aes(color = Chain)) +
          facet_wrap_paginate(Variable~Method, scales = "free", ncol = 2, nrow = (ncol(dataset$original)%%3), page = i,
                              labeller = labeller(Variable = var.labs)) +
          ylab(NULL) +
          #theme(legend.position = "none") +
          ggtitle("Mean and Variance through Imputation Chains")
        if (print_plot == TRUE) {
          print(plot)
        }
        if (save_plot == TRUE) {
          title <- paste("iterations",i,".png", sep = "")
          ggsave(title, plot = plot, height = 10*(ncol(dataset$original)%%3), width = 30, units = "cm")
        }
      } else {
        plot <- ggplot(data = plotting.data, aes(x = Iteration , y = value)) +
          geom_line(aes(color = Chain)) +
          facet_wrap_paginate(Variable~Method, scales = "free", ncol = 2, nrow = 3, page = i,
                              labeller = labeller(Variable = var.labs)) +
          ylab(NULL) +
          #theme(legend.position = "none") +
          ggtitle("Mean and Variance through Imputation Chains")
        if (print_plot == TRUE) {
          print(plot)
        }
        if (save_plot == TRUE) {
          title <- paste("iterations",i,".png", sep = "")
          ggsave(title, plot = plot, height = 30, width = 30, units = "cm")
        }
      }
    }
  }
}