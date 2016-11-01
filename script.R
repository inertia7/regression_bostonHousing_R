
# RUN EACH OF COMMANDS ON YOUR CONSOLE:

# install.packages("usdm")
# install.packages("car")
# install.packages("MASS")
# install.packages("DAAG")
# install.packages("lmtest")
# install.packages("ggplot2")
# install.packages("ggfortify")
# install.packages("GGally")

library(usdm) # for testing collinearity 
library(car) # for testing outliers
library(MASS) # for testing studentized residuals
library(DAAG) # for cross validation of model
library(lmtest) # for checking homoskedasticity/heteroskedasticity
library(ggplot2) # Use for visuals
library(ggfortify)
library(GGally)

# LOAD DATA
data(Boston) 
help(Boston)
attach(Boston)

boston.df <- data.frame(Boston)

boston.df$chas <- as.factor(boston.df$chas)
boston.df$rad <- as.factor(boston.df$rad)

# Since rad seems to be an ordinal variable we add levels 
# just to be safe!    

levels(boston.df$rad) <- c(1, 2, 3, 4, 5, 6, 7, 8, 24)

# ECPLORATORY ANALYSIS

# USING STR TO GET AN IDEA OF THE STRUCTURE OF OUR DATA SET
str(boston.df)

# SCATTERPLOT MATRICE USING GGPAIRS
pairs <- ggpairs(boston.df, columns = c(1, 2, 3, 4, 5, 14),
	lower=list(continuous=wrap("smooth",
		colour="turquoise4")),
	diag=list(continuous=wrap("barDiag",
		fill="turquoise4")))  + 
theme(panel.background = element_rect(fill = "gray98"),
	axis.line.y = element_line(colour="gray"),
    axis.line.x = element_line(colour="gray"))
pairs


pairs1 <- ggpairs(boston.df, columns = c(6, 7, 8, 9, 10, 14),
	lower=list(continuous=wrap("smooth", 
		colour="turquoise4")),
	diag=list(continuous=wrap("barDiag", 
		fill="turquoise4"))) + 
theme(panel.background = element_rect(fill = "gray98"),
	axis.line.y = element_line(colour="gray"),
	axis.line.x = element_line(colour="gray"))
pairs1

pairs3 <- ggpairs(boston.df, columns=c(11, 12, 13,14), 
	lower=list(continuous=wrap("smooth",
		colour="turquoise4")),
	diag=list(continuous=wrap("barDiag", 
		fill="turquoise4"))) +
theme(panel.background = element_rect(fill = "gray98"),
	axis.line.y = element_line(colour="black"),
    axis.line.x = element_line(colour="black"))
pairs3

# VARIABLE INTERACTIONS USING GGPLOT2
taxMedv <- ggplot(boston.df, aes(tax, lstat, colour = rad)) + 
theme(panel.background = element_rect(fill = "gray98"),
	axis.line   = element_line(colour="black"),
    axis.line.x = element_line(colour="gray"),
    axis.line.y = element_line(colour="gray"))
taxMedv + geom_point()

# POSSIBLE OUTLIERS
taxPtratio <- ggplot(boston.df, aes(tax, ptratio, colour = ptratio))
taxPtratio + geom_point() + ggtitle("TAX vs. PTRATIO Scatterplot") +
  theme(panel.background = element_rect(fill = "gray98"),
  	axis.line   = element_line(colour="black"),
  	axis.line.x = element_line(colour="gray"),
  	axis.line.y = element_line(colour="gray")) 

# CHECKING THE DISTRIBUTION OF MEDV
medvHist <- ggplot(boston.df, aes(medv))
medvHist + geom_histogram(aes(y = ..density.., fill = ..count..),
                          colour = 'white', bins = 25) + 
  geom_density() +
  scale_fill_gradient("Count", low = "black", 
    high = "turquoise4") +
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) + 
  ggtitle("Histogram of MEDV")

# WE SAW IT APPROPRIATE TO DO A LOG TRANSFORMATION TO MEDV

# First we create a data frame with the log transformation 
logMedv <- data.frame(log(boston.df$medv))

# Next we change the name of the column to make it easy
# for us to call in our ggplot2 visual
colnames(logMedv) <- c("logMedv")

logMedvHist <- ggplot(logMedv, aes(logMedv))
logMedvHist + geom_histogram(aes(y=..density.., fill=..count..), 
                             colour = 'white', bins = 25) + 
  geom_density() +
  scale_fill_gradient("Count", low = "black", 
                      high = "turquoise4") +
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) + 
  ggtitle("Histogram of log(MEDV)")

# MODELING
# FIRST MODEL
fit1 <- lm(log(medv) ~ ., data = boston.df)
summary(fit1)

bptest(fit1)

vif(fit1)

# SECOND MODEL REMOVING VARIABLES WITH LARGE VIF VALUES
fit2 <- update(fit1, ~ . - tax)

vif(fit2)

summary(fit2)

# THIRD MODEL
fit3 <- update(fit2, ~ . - age - indus - rad)

summary(fit3)

# DIAGNOSING FOR OUTLIERS 
outlierTest(fit3, cutoff = Inf, n.max = 15)

# REMOVING OUTLIERS
boston.df <- boston.df[-c(413, 372, 369, 373, 402, 375, 490, 506, 215, 401),]

fit4 <- lm(log(medv) ~ . - tax - age - indus - rad, data = boston.df)

summary(fit4)

# RESIDUAL DIAGNOSTICS FOR OUR FOURTH MODEL
autoplot(fit4, label.size = 3, shape = 21, data=boston.df, colour='turquoise4') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) + 
  ggtitle("Residual Diagnostics for fit4")



residBar <- ggplot(data=fit4, aes(residuals(fit4))) + 
  geom_histogram(aes(y =..density..), 
                 col="black", fill="white") +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Histogram of Residuals for fit4") 
residBar

# ADDING NON-LINEAR TERMS
fit5 <- lm(log(medv) ~ . - tax - age - indus - rad + I(lstat^2), data = boston.df)

summary(fit5)

# RESIDUAL DIAGNOSTICS FOR OUR FIFTH MODEL
autoplot(fit5, label.size = 3, shape = 21, data=boston.df, colour='turquoise4') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) + 
  ggtitle("Residual Diagnostics for fit5")

  # SIXTH MODEL MORE NON-LINEARITY 
fit6 <- lm(log(medv) ~ . - tax - age - indus - rad + I(lstat^2) + I(lstat^3), data = boston.df)

summary(fit6)

# RESIDUAL DIAGNOSTICS FOR SIXTH MODEL
autoplot(fit6, label.size = 3, shape = 21, data=boston.df, colour='turquoise4') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) + 
  ggtitle("Residual Diagnostics for fit6")

# CROSS VALIDATION

# CV FOR OUR FOURTH MODEL
par(mfrow = c(1,1))
fit4_CV <- suppressWarnings(CVlm(data = boston.df, form.lm = formula(fit4), m = 10, dots = FALSE, seed = 1, plotit = c("Observed", "Residual"),main = "Cross-validation for fit4", legend.pos="topleft"))
attr(fit4_CV, 'ms')

# CV FOR OUR FIFTH MODEL 
par(mfrow = c(1,1))
fit5_CV <- suppressWarnings(CVlm(data = boston.df, form.lm = formula(fit5), m = 10, dots = FALSE, seed = 1, plotit = c("Observed", "Residual"), main = "Cross-validation for fit5", legend.pos="topleft"))
attr(fit5_CV, 'ms')

# CV FOR OUR SIXTH MODEL
par(mfrow = c(1,1))
fit6_CV <- suppressWarnings(CVlm(data = boston.df, form.lm = formula(fit6), m = 10, dots = FALSE, seed = 1, plotit = c("Observed", "Residual"), main = "Cross-validation for fit6", legend.pos="topleft"))

# MORE RESIDUAL DIAGNOSTIC PLOT FOR OUR MODEL THAT BEST FIT
autoplot(fit5, which = 1:6, ncol = 3, label.size = 1, colour='turquoise4') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray"))
