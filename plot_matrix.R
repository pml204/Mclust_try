plot_matrix <- function(dataset, save_plot = FALSE, imputations = NULL) {
  require(tidyverse)
  require(ggplot2)
  require(GGally)
  
  
  plots <- vector(mode = "list", length = 4)
  position <- matrix(1:(ncol(dataset$original)^2), ncol(dataset$original), ncol(dataset$original), byrow = TRUE)
  
  if (is.null(imputations)) {                 #if not specified imputations to show
    imputations  <- rep(1:dataset$imputation)
  }
  
  for (row in 1:ncol(dataset$original)) {
    for (column in 1:ncol(dataset$original)) {
      if (row < column) {        #upper area of the matrix
        
        test <- dataset$mclust$data[,c(row,column)] %>%
          cbind(Class = dataset$mclust$classification)
        colnames(test) <- c("var1","var2","Class")
        test <- as.data.frame(test) %>%
          mutate(Class = factor(Class))
        
        plot <- ggplot(test, aes(x = var1, y = var2)) +
          geom_point(aes(color = Class), shape = 4, size = 1)
        
      } else if (row > column) {      #lower area of the matrix
        if (is.null(imputations)) {
          imputations  <- rep(1:dataset$imputation)
        }
        
        data1 <- data.frame(a = dataset$original[complete.cases(dataset$original),c(column,row)]) %>%
          cbind(Status="Original")
        colnames(data1) <- c("var1","var2","Status")
        data2 <- data.frame(var1 = unlist(data.frame(dataset$imp[[column]][,imputations])),
                            var2 = unlist(data.frame(dataset$imp[[row]][,imputations]))) %>%
          cbind(Status="Imputed")
        colnames(data1) <- c("var1","var2","Status")
        viz <- rbind(data1,data2) %>%
          mutate(Status = factor(Status))

        plot <- ggplot(viz, aes(x = var1, y = var2)) +
          geom_point(aes(colour = Status), size = 1, alpha = 0.4)

      } else {        # diagonal
        
        data1 <- data.frame(a = dataset$original[complete.cases(dataset$original[,row]),row]) %>%
          cbind(Status="Original")
        data2 <- data.frame(a = unlist(data.frame(dataset$imp[[row]]))) %>%
          cbind(Status="Imputed")
        viz <- rbind(data1,data2)  %>%
          mutate(Status = factor(Status))
        
        plot <- ggplot(viz, aes(x = a)) +
          geom_density(aes(fill = Status), alpha = 0.3)
        
      }
      plots[[position[row,column]]] <- plot
    }
  }
  
  final <- ggmatrix(plots, ncol(dataset$original), ncol(dataset$original),
           xAxisLabels = colnames(dataset$original),
           yAxisLabels = colnames(dataset$original),
           #title = "Mclust Classifiers and Imputed data",
           legend = 1) +
    theme(legend.position = "bottom")
  
  print(final)
  
  if (save_plot == TRUE) {
    ggsave("matrix.png", plot = final, height = 20, width = 27, units = "cm")
  }
  
}