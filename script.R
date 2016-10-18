#######
#######
#######
#######
#######
#######
#######
####### Linear Models for Boston Housing
#######
#######
#######
#######
#######
#######
#######



##
####
######
#     LOADING DATA, PACKAGES
######
####
##

print("Linear Regression for Boston Housing")
wd <- getwd()
parent <- getwd()
setwd(wd)
print(parent)


# install.packages("usdm")
# install.packages("car")
# install.packages("MASS")
# install.packages("DAAG")
# install.packages("lmtest")
# install.packages("ggplot2")
# install.packages("ggfortify")
# install.packages("GGally")

# loading libraries
library(usdm) # for testing collinearity
library(car) # for testing outliers
library(MASS) # for testing studentized residuals
library(DAAG) # for cross validation of model
library(lmtest) # for checking homoskedasticity / heteroskedasticity
require(ggplot2)
library(ggfortify)
library(GGally)

# outputting work

pdf("regression_Boston.pdf")


###########################################################
print(" ")
print(" ")
print(" ")
###########################################################

##
####
######
#     TESTING that data loads properly
######
####
##

head(Boston)
str(Boston)



###########################################################
print(" ")
print(" ")
print(" ")
###########################################################


##
####
######
#     SETTING UP DATA FRAME
######
####
##

boston.df <- data.frame(Boston)
boston.df$chas <- as.factor(boston.df$chas)
boston.df$rad <- as.factor(boston.df$rad)
levels(boston.df$rad) <- c(1, 2, 3, 4, 5, 6, 7, 8, 24)
str(boston.df)

##
####
######
#     EXPLORATORY ANALYSIS
######
####
##


pairs <- ggpairs(boston.df, columns = c(1, 2, 3, 4, 5, 14), 
                 lower=list(continuous=wrap("smooth", colour="turquoise4")),
                 diag=list(continuous=wrap("barDiag", fill="turquoise4")))  + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray"))
pairs

pairs1 <- ggpairs(boston.df, columns = c(6, 7, 8, 9, 10, 14),
                  lower=list(continuous=wrap("smooth", colour="turquoise4")),
                  diag=list(continuous=wrap("barDiag", fill="turquoise4"))) + 
  theme(panel.background = element_rect(fill = "gray98"),
                  axis.line.y = element_line(colour="gray"),
                  axis.line.x = element_line(colour="gray"))
pairs1

pairs3 <- ggpairs(boston.df, columns=c(11, 12, 13,14), 
                  lower=list(continuous=wrap("smooth", colour="turquoise4")),
                  diag=list(continuous=wrap("barDiag", fill="turquoise4"))) +
  theme(panel.background = element_rect(fill = "gray98"),
                  axis.line.y = element_line(colour="black"),
                  axis.line.x = element_line(colour="black"))
pairs3


##
####
######
#     VARIABLE INTERACTIONS
######
####
##



taxMedv <- ggplot(boston.df, aes(tax, lstat, colour = rad)) + 
theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray"))
taxMedv + geom_point()


taxPtratio <- ggplot(boston.df, aes(tax, ptratio, colour = ptratio))
taxPtratio + geom_point() + ggtitle("TAX vs. PTRATIO Scatterplot") +
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) 
        
medvHist <- ggplot(boston.df, aes(medv))
medvHist + geom_histogram(aes(y = ..density.., fill = ..count..),
                          colour = 'white', bins = 25) + geom_density() +
  scale_fill_gradient("Count", low = "black", high = "turquoise4") +
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) + 
  ggtitle("Histogram of MEDV")

# First we create a data frame with the log transformation 
logMedv <- data.frame(log(boston.df$medv))

# Next we change the name of the column to make it easy
# for us to call in our ggplot2 visual
colnames(logMedv) <- c("logMedv")

logMedvHist <- ggplot(logMedv, aes(logMedv))
logMedvHist + geom_histogram(aes(y=..density.., fill=..count..), colour = 'white', bins = 25) + 
  geom_density() +
  scale_fill_gradient("Count", low = "black", 
                      high = "turquoise4") +
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) + 
  ggtitle("Histogram of log(MEDV)")


##
####
######
#     BUILDING LINEAR MODELS
######
####
##

fit1 <- lm(log(medv) ~ ., data = boston.df)
summary(fit1)

bptest(fit1)

vif(fit1)


fit2 <- update(fit1, ~ . - tax)
vif(fit2)
summary(fit2)

fit3 <- update(fit2, ~ . - age - indus - rad)
summary(fit3)

outlierTest(fit3, cutoff = Inf, n.max = 15)



boston.df <- boston.df[-c(413, 372, 369, 373, 402, 375, 490, 506, 215, 401),]

fit4 <- lm(log(medv) ~ . - tax - age - indus - rad, data = boston.df)
summary(fit4)


autoplot(fit4, label.size = 3, shape = 21, data=boston.df, colour='turquoise4') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) + 
  ggtitle("Residual Diagnostics for fit4")
  
  
  
residBar <- ggplot(data=fit4, aes(residuals(fit4))) + 
  geom_histogram(aes(y =..density..), col="black", fill="white") +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Histogram of Residuals for fit4") 
residBar

fit5 <- lm(log(medv) ~ . - tax - age - indus - rad + I(lstat^2), data = boston.df)
summary(fit5)

autoplot(fit5, label.size = 3, shape = 21, data=boston.df, colour='turquoise4') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) + 
  ggtitle("Residual Diagnostics for fit5")
  
fit6 <- lm(log(medv) ~ . - tax - age - indus - rad + I(lstat^2) + I(lstat^3), data = boston.df)
summary(fit6)  

autoplot(fit6, label.size = 3, shape = 21, data=boston.df, colour='turquoise4') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) + 
  ggtitle("Residual Diagnostics for fit6")

##
####
######
#     CROSS VALIDATION
######
####
##

par(mfrow = c(1,1))
fit4_CV <- suppressWarnings(CVlm(data = boston.df, form.lm = formula(fit4), m = 10, dots = FALSE, seed = 1, plotit = c("Observed", "Residual"),main = "Cross-validation for fit4", legend.pos="topleft"))
attr(fit4_CV, 'ms')

par(mfrow = c(1,1))
fit5_CV <- suppressWarnings(CVlm(data = boston.df, form.lm = formula(fit5), m = 10, dots = FALSE, seed = 1, plotit = c("Observed", "Residual"), main = "Cross-validation for fit5", legend.pos="topleft"))
attr(fit5_CV, 'ms')

par(mfrow = c(1,1))
fit6_CV <- suppressWarnings(CVlm(data = boston.df, form.lm = formula(fit6), m = 10, dots = FALSE, seed = 1, plotit = c("Observed", "Residual"), main = "Cross-validation for fit6", legend.pos="topleft"))

autoplot(fit5, which = 1:6, ncol = 3, label.size = 1, colour='turquoise4') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray"))  
