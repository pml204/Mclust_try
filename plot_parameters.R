plot_parameter <- function(dataset, plot_columns = 7, save_plot = FALSE) {
  require(ggplot2)
  require(tidyverse)
  
  if (ncol(dataset$parameter) != 4) {
    
    
    parameter <- dataset$parameter %>%
      mutate(Imputation = factor(Imputation))
    
    Variable.labs <- NULL
    for (i in 1:dataset$imputation) {
      Variable.labs[i] <- paste("Imputation", i)
    }
    names(Variable.labs) <- rep(1:dataset$imputation)
    
    plot <- ggplot(data = parameter, aes(x = Iteration , y = Parameter)) +
      geom_point() +
      geom_line(aes(color = Imputation)) +
      #facet_wrap(~Imputation, ncol = 1,
      #           labeller = labeller(Imputation = Variable.labs)) +
      ylab("Number of Components") +
      #scale_x_continuous(breaks = seq(min(dataset$iteration)-1, max(dataset$iteration)+1, by = 2)) +
      ggtitle("Number of Components Used in mclust")
    print(plot)
    
    
    
    
  } else {
  
    parameter <- dataset$parameter %>%
      mutate(Imputation = factor(Imputation))       #ease of colouring
    Variable.labs <- colnames(dataset$original)       #custom labels
    names(Variable.labs) <- rep(1:(ncol(dataset$original)))
    
    plot <- ggplot(data = parameter, aes(x = Iteration , y = Parameter)) +
      geom_line(aes(color = Imputation)) +
      geom_point() +
      facet_wrap(~Variable, ncol = plot_columns,
                 labeller = labeller(Variable = Variable.labs)) +
      ylab("Number of Components") +
      #scale_x_continuous(breaks = seq(min(dataset$iteration)-1, max(dataset$iteration)+1, by = 2)) +
      ggtitle("Number of Components Used in mclust")
    print(plot)
    
    
  }
  
  if (save_plot == TRUE) {
    ggsave("parameters.png", plot = plot, height = 20, width = 27, units = "cm")
  }

}