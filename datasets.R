#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

# # Linear Term                        # I SUGGEST 1 PARAMETER (MCLUST WEIRDLY LOCKS WITH MORE)
# library(MASS)
# # Correlation Matrix for X1 and X2
# Sigma <- matrix(c(1,   0.8,
#                   0.8, 1), 2,2)
# # X1 Mean
# mu1 <- 0
# #X2 Mean
# mu2 <- 0
# 
# # B0 - Intercept
# B0 <- 3
# # B1
# B1 <- 0.8
# # B2
# B2 <- -0.3
# # Draw values from the Multivariate Distribution (5k values)
# set.seed(123)
# dataset <- as.data.frame(mvrnorm(n=5000, mu = c(mu1,mu2), Sigma))
# colnames(dataset) <- c("X1","X2")
# # Add a Y with structure from both variables.
# set.seed(123)
# y <- B0 + B1*dataset$X1 + B2*dataset$X2 + rnorm(length(dataset$X1),mean=0,sd=0.3)
# dataset <- cbind(dataset, Y=y)

#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

# # Quadratic Term                        # I SUGGEST 50:60 PARAMETERS (TAKES LONG TIME)
# library(MASS)
# # Correlation Matrix for X1 and X2
# Sigma <- matrix(c(1,   0.8,
#                   0.8, 1), 2,2)
# # X1 Mean
# mu1 <- 0
# #X2 Mean
# mu2 <- 0
# 
# # B0 - Intercept
# B0 <- 3
# # B1
# B1 <- 0.8
# # B2
# B2 <- -0.3
# # Draw values from the Multivariate Distribution (5k values)
# set.seed(123)
# dataset <- as.data.frame(mvrnorm(n=5000, mu = c(mu1,mu2), Sigma))
# colnames(dataset) <- c("X1","X2")
# # Add a Y with structure from both variables.
# set.seed(123)
# y <- B0 + B1*(dataset$X1^2) + B2*dataset$X2 + rnorm(length(dataset$X1),mean=0,sd=0.3)
# dataset <- cbind(dataset, Y=y)


#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

# # Interaction Term                        # I SUGGEST 50:60 PARAMETERS (TAKES LONG TIME)
# library(MASS)
# # Correlation Matrix for X1 and X2
# Sigma <- matrix(c(1,   0.8,
#                   0.8, 1), 2,2)
# # X1 Mean
# mu1 <- 0
# #X2 Mean
# mu2 <- 0
# # B0 - Intercept
# B0 <- 3
# # B1
# B1 <- 0.4
# # B2
# B2 <- -0.3
# # B3
# B3 <- 0.2
# # Draw values from the Multivariate Distribution (5k values)
# set.seed(123)
# dataset <- as.data.frame(mvrnorm(n=5000, mu = c(mu1,mu2), Sigma))
# colnames(dataset) <- c("X1","X2")
# # Add a Y with structure from both variables.
# set.seed(123)
# y <- B0 + B1*dataset$X1 + B2*dataset$X2 + B3*(dataset$X1*dataset$X2) + rnorm(1,mean=0,sd=0.3)
# dataset <- cbind(dataset, Y=y)


#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

# 2 Parameter Clust                                # I SUGGEST 2 PARAMETERS
set.seed(123)
x1 <- rnorm(2000, mean = c(-10,25), sd= c(15,15))
x2 <- rnorm(2000, mean = c(25,12.5), sd= c(3,5))
dataset <- as.data.frame(cbind(X1 = x1, X2 = x2))




#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################



# Missing Values

# missing data MCAR
library(missMethods)
# introducing missing data in some variables
# randomly (MCAR) choose 30% of rows to have missing data
rows <- sample(nrow(dataset), as.numeric(nrow(dataset)*0.3))
# randomly delete 20% of data in the chosen rows
dataset.mcar.or <- delete_MCAR(dataset[rows,], 0.2) 
dataset.mcar.or <- rbind(dataset.mcar.or, dataset[-rows,])
rownames(dataset.mcar.or) <- 1:nrow(dataset.mcar.or)
# retrieving the data which is gone
#matrix where values are missing:   imp[["where"]]
dataset.missing <- dataset[rows,]
dataset.missing <- rbind(dataset.missing, dataset[-rows,])
dataset.missing[!is.na(dataset.mcar.or)] <- NA
rownames(dataset.missing) <- 1:nrow(dataset.missing)



#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

#GGPAIRS


# ggpairs with coloured missing data
library(GGally)
library(tidyverse)
data1 <- dataset[-rows,] %>%
  cbind(Status="Present")
data2 <- dataset[rows,] %>%
  cbind(Status="Missing")
viz <- rbind(data1,data2)

ggpairs(viz, columns = 1:(ncol(viz)-1), 
        legend = 1,
        title = "MCAR: 20% Missing Data",
        mapping = ggplot2::aes(colour=Status),
        lower = list(continuous = wrap("points",alpha = 0.5, size=0.9)),
        diag = list(continuous = wrap("densityDiag", alpha=0.3))) +
  theme(legend.position = "bottom")


#ggsave("interaction_pairs.png", height = 20, width = 27, units = "cm")



#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

# Imputation


source("mclust_version1.R")
source("plot_parameters.R")
source("plot_bar_imp.R")
source("plot_iterations.R")
#imputation
imput1 <- mclust_version1(dataset.mcar.or, imputations = 3, maxit = 30,G= 2)
#parameter plot
plot_parameter(imput1)
#boxplot
plot_bar_imp(imput1)
#iterations
plot_iterations(imput1)


