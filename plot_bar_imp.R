plot_bar_imp <- function(dataset, print_plot = TRUE, save_plot = FALSE) {
  require(tidyverse)
  require(ggplot2)
  #outcome of function
  grouped_values <- NULL
  
  for (variable in ncol(dataset$original):1) {
    values <- dataset$imp[[variable]]
    
    for (column in 1:dataset$imputation) {
      imp <- cbind(values[,column], Imputation = column)
      grouped_values[[variable]] <- rbind(grouped_values[[variable]], imp)
    }
    imp <- cbind(dataset$original[complete.cases(dataset$original[,variable]),variable], Imputation = 0)
    grouped_values[[variable]] <- rbind(imp, grouped_values[[variable]])
    colnames(grouped_values[[variable]])[1] <- "values"
  }
  
  for (variable in 1:ncol(dataset$original)) {
    plotting.data <- as.data.frame(grouped_values[[variable]]) %>%
      mutate(Imputation = factor(Imputation),
             Highlight = ifelse(Imputation==0,"Highlighted","Normal"))
    
    title <- paste("Boxplot for Original vs Imputed Values for",
                           colnames(dataset$original)[variable])
    
    plot <- ggplot(plotting.data, aes(x=Imputation,y=values)) +
      geom_boxplot(aes(fill = Highlight, color = Highlight)) +
      scale_fill_manual(values = c("springgreen3","lightblue3")) +
      scale_color_manual(values = c("springgreen4","cornflowerblue")) +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 90)) +
      ggtitle(title)
    
    if (print_plot == TRUE) {
      print(plot)
    }
    
    save <- paste("box_",
                   colnames(dataset$original)[variable],
                  ".png")
    save <- gsub(" ", "", save, fixed = TRUE)
    
    if (save_plot == TRUE) {
      ggsave(save, plot = plot, height = 20, width = 27, units = "cm")
    }
  }
}