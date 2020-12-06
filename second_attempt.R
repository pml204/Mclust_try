

#######################################################
#######################################################
#######################################################
###### Generating Synthetic Data for testing MICE
######  1. y ~ B0 + B1*X1 + B2*X2 + noise
######
######    X1,X2 <- normal distribution
######    X1,X2 mean <- defined
######    cor(X1,X2) -> defined
######
#######################################################
#######################################################
#######################################################

library(MASS)

# Correlation Matrix for X1 and X2
Sigma <- matrix(c(1,   0.8,
                  0.8, 1), 2,2)

# X1 Mean
mu1 <- 0

#X2 Mean
mu2 <- 0

# B0 - Intercept
B0 <- 3
# B1
B1 <- 0.8
# B2
B2 <- -0.3


# Draw values from the Multivariate Distribution (5k values)
set.seed(123)
dataset <- as.data.frame(mvrnorm(n=5000, mu = c(mu1,mu2), Sigma))
colnames(dataset) <- c("X1","X2")

# Add a Y with structure from both variables.
set.seed(123)
y <- B0 + B1*(dataset$X1^2) + B2*dataset$X2 + rnorm(length(dataset$X1),mean=0,sd=0.3)



dataset <- cbind(dataset, Y=y)
#dataset <- cbind(Y=y, dataset)



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






# ggpairs with coloured missing data
library(tidyverse)
library(GGally)

data1 <- dataset[-rows,] %>%
  cbind(Status="Present")
data2 <- dataset[rows,] %>%
  cbind(Status="Missing")
viz <- rbind(data1,data2)

ggpairs(viz, columns = 1:3, 
        legend = 1,
        title = "MCAR: 20% Missing Data",
        mapping = ggplot2::aes(colour=Status),
        lower = list(continuous = wrap("points",alpha = 0.5, size=0.9)),
        diag = list(continuous = wrap("densityDiag", alpha=0.3))) +
  theme(legend.position = "bottom")


ggsave("pairs.png", height = 20, width = 27, units = "cm")


########################################################################
########################################################################
####### Issues:
####### - Only plots X1^2 because with X1, Sigma has a value above 1 for variance
########################################################################
########################################################################
########################################################################
########################################################################
library(mclust)

#fill missing data with means

test.data <- dataset.mcar.or

#position of missing data
test.data.missing <- is.na(test.data)
test.data.present <- !is.na(test.data)


# function for any number of variables
for (variable in 1:ncol(test.data)) {
  test.data[,variable] = ifelse(is.na(test.data[,variable]),
                                ave(test.data[,variable], FUN = function(x) mean(x, na.rm = TRUE)),
                                test.data[,variable])
}


# Mclust / Mice model 
##
## Iterations

imputations = 10
##
## Numbers of Iterations for the creation of one dataset


##
##  model has one cycle around variables, one iteration changing the "missing" values

library(condMVNorm)

imp1 <- dataset %>%
  mutate(Imputation = 0)



for (cycle in 1: imputations) {
  
  imputing <- test.data
  
  for (iteration in 1:10) {
    
    
    for (variable in 1:ncol(test.data)) {
      
      # run mclust best BIC
      model.mclustBIC <- mclustBIC(test.data, G = 1:30)
      
      # run best BIC model
      model.mclust <- Mclust(test.data, x = model.mclustBIC)
      
      
      
      # collect sigma and mu from mclust
      sigma <- model.mclust$parameters$variance$sigma
      mu <- model.mclust$parameters$mean
      
      
      #position of missing data
      position <- which(test.data.missing[,variable])
      
      for (row in position) {
        
        
        draw <- sample(1:model.mclust$G,1,replace = FALSE,prob = model.mclust$parameters$pro)
        # random draw of sigma
        var <- sigma[,,draw]
        #random draw of mu
        mu.it <- mu[,draw]
        #variable being imputed now
        dependent <- variable
        #position of given variables
        given <- which(test.data.present[row,])
        #values of given variables
        x.given <- as.numeric(test.data[row,given])
        
        
        # conditional mean/variance 
        test <- condMVN(mean=mu.it, sigma=var, dependent=dependent, given=given,
                X.given=x.given)
        
        imputing[row,variable] <- rnorm(1,test$condMean, test$condVar)
      }
      
    }
  
  }
  imputing <- imputing %>%
    mutate(Imputation = cycle)
  
  imp1 <- rbind(imp1, imputing)

}

###########################################################################
###########################################################################
###########################################################################
###########################################################################

# True/False location
data.missing <- matrix( rep( t(test.data.missing), imputations+1), ncol = ncol(data.missing), byrow = TRUE)


imp1[,1:3][!data.missing] <- NA

#  checking what is coming out right now


#aggregate real and imputed values of X1
X1.results.1  <- as.data.frame(cbind(Real = dataset.missing$X1,Predicted = imp1$X1,
                                     Residuals = dataset.missing$X1-imp1$X1,
                                     Imputation = imp1$Imputation))
#variable containing all iterations
X1.results.1 <- X1.results.1[complete.cases(X1.results.1),]


# only show the first 3 imputed datasets
if (imputations > 3) {
  res.plot <- X1.results.1[as.numeric(X1.results.1$Imputation) < 4, ]
  res.plot <- res.plot[as.numeric(res.plot$Imputation) > 0, ] %>%
    mutate(Imputation = factor(Imputation))
  
  ggplot(res.plot, aes(x=Real,y=Predicted)) +
    geom_point(aes(colour=Imputation), size = 1) +
    geom_abline(intercept = 0,slope = 1) +
    stat_smooth(method = "gam", col = "red") +
    ggtitle("MCAR: X1 Residual Plot (20% missing data")
} else {
  
  ggplot(X1.results.1, aes(x=Real,y=Predicted)) +
    geom_point(aes(colour=Imputation), size = 1) +
    geom_abline(intercept = 0,slope = 1) +
    stat_smooth(method = "gam", col = "red") +
    ggtitle("MCAR: X1 Residual Plot (20% missing data")
  
}


ggsave("x1.png", height = 20, width = 27, units = "cm")


Box.all <- X1.results.1 %>%
  mutate(Imputation = factor(Imputation),
         Highlight = ifelse(Imputation==0,"Highlighted","Normal"))


ggplot(Box.all, aes(x=Imputation,y=Predicted)) +
  geom_boxplot(aes(fill = Highlight, color = Highlight)) +
  scale_fill_manual(values = c("springgreen3","lightblue3")) +
  scale_color_manual(values = c("springgreen4","cornflowerblue")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("MCAR: X1 Residual Plot (20% missing data")


ggsave("box_x1.png", height = 20, width = 27, units = "cm")



#aggregate real and imputed values of X2


X2.results.1  <- as.data.frame(cbind(Real = dataset.missing$X2,Predicted = imp1$X2,
                                     Residuals = dataset.missing$X2-imp1$X2,
                                     Imputation = imp1$Imputation))
#variable containing all iterations
X2.results.1 <- X2.results.1[complete.cases(X2.results.1),]


# only show the first 3 imputed datasets
if (imputations > 3) {
  res.plot <- X2.results.1[as.numeric(X2.results.1$Imputation) < 4, ]
  res.plot <- res.plot[as.numeric(res.plot$Imputation) > 0, ] %>%
    mutate(Imputation = factor(Imputation))
  
  ggplot(res.plot, aes(x=Real,y=Predicted)) +
    geom_point(aes(colour=Imputation), size = 1) +
    geom_abline(intercept = 0,slope = 1) +
    stat_smooth(method = "gam", col = "red") +
    ggtitle("MCAR: X2 Residual Plot (20% missing data")
} else {
  
  ggplot(X2.results.1, aes(x=Real,y=Predicted)) +
    geom_point(aes(colour=Imputation), size = 1) +
    geom_abline(intercept = 0,slope = 1) +
    stat_smooth(method = "gam", col = "red") +
    ggtitle("MCAR: X2 Residual Plot (20% missing data")
  
}


ggsave("x2.png", height = 20, width = 27, units = "cm")


Box.all <- X2.results.1 %>%
  mutate(Imputation = factor(Imputation),
         Highlight = ifelse(Imputation==0,"Highlighted","Normal"))


ggplot(Box.all, aes(x=Imputation,y=Predicted)) +
  geom_boxplot(aes(fill = Highlight, color = Highlight)) +
  scale_fill_manual(values = c("springgreen3","lightblue3")) +
  scale_color_manual(values = c("springgreen4","cornflowerblue")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("MCAR: X2 Residual Plot (20% missing data")


ggsave("box_x2.png", height = 20, width = 27, units = "cm")




#aggregate real and imputed values of Y


Y.results.1  <- as.data.frame(cbind(Real = dataset.missing$Y,Predicted = imp1$Y,
                                     Residuals = dataset.missing$Y-imp1$Y,
                                     Imputation = imp1$Imputation))
#variable containing all iterations
Y.results.1 <- Y.results.1[complete.cases(Y.results.1),]


# only show the first 3 imputed datasets
if (imputations > 3) {
  res.plot <- Y.results.1[as.numeric(Y.results.1$Imputation) < 4, ]
  res.plot <- res.plot[as.numeric(res.plot$Imputation) > 0, ] %>%
    mutate(Imputation = factor(Imputation))
  
  ggplot(res.plot, aes(x=Real,y=Predicted)) +
    geom_point(aes(colour=Imputation), size = 1) +
    geom_abline(intercept = 0,slope = 1) +
    stat_smooth(method = "gam", col = "red") +
    ggtitle("MCAR: Y Residual Plot (20% missing data")
} else {
  
  ggplot(Y.results.1, aes(x=Real,y=Predicted)) +
    geom_point(aes(colour=Imputation), size = 1) +
    geom_abline(intercept = 0,slope = 1) +
    stat_smooth(method = "gam", col = "red") +
    ggtitle("MCAR: Y Residual Plot (20% missing data")
  
}


ggsave("y.png", height = 20, width = 27, units = "cm")


Box.all <- Y.results.1 %>%
  mutate(Imputation = factor(Imputation),
         Highlight = ifelse(Imputation==0,"Highlighted","Normal"))


ggplot(Box.all, aes(x=Imputation,y=Predicted)) +
  geom_boxplot(aes(fill = Highlight, color = Highlight)) +
  scale_fill_manual(values = c("springgreen3","lightblue3")) +
  scale_color_manual(values = c("springgreen4","cornflowerblue")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("MCAR: Y Residual Plot (20% missing data")


ggsave("box_y.png", height = 20, width = 27, units = "cm")


