getwd()
setwd("C:/Users/chaitanya vema/Documents")
getwd()
# Loading the rents dataset into R console
rent <- read.csv("NEW_DATA.csv")
# showing the structure in R console
str(rent)
head(rent)
# Now I am building the linear regression model using the rents and populaion of the year 2018
# The dataset contains all the rental prices and the population of 2018
linear_model <- lm(X2018 ~ POPULATION, data = rent)
linear_model
summary(linear_model)
# The estimates of the beta coefficients
# the standard errors (SE), which defines the accuracy of beta coefficients. 
# For a given beta coefficient, the SE reflects how the coefficient varies under 
# repeated sampling. It can be used to compute the confidence intervals and the t-statistic.
# the t-statistic and the associated p-value, which defines the statistical significance of the beta coefficients.


# Plotting Income and Rent variable to see relationship between the response(rent) and
# predictor (Population) variable

plot(rent$X2018,rent$POPULATION,
     xlab="X2018",
     ylab="POPULATION",
     main = "Scatter plot showing regression line
     for Rent predicted from Income")
abline(linear_model)
# Graph shows a there is some relationship between rent and population variable

cor(rent$X2018,rent$POPULATION)
# Examining the 95% confidence intervals of the model

confint(linear_model)

# Scatter plots helps to visualise any linear relationships between the 
# dependent (response) rent variable and independent (predictor) population variables



scatter.smooth(x = rent$X2018, 
               y = rent$POPULATION, 
               main = "X2018 ~ POPULATION",
               xlab = "X2018",
               ylab = "POPULATION")
# Box Plot
par(mfrow = c(1, 2))

boxplot(rent$X2018, main = "X2018", sub = paste("Outlier rows: ", boxplot.stats(rent$X2018)$out))
boxplot(rent$X2018, main = "POPULATION", sub = paste("Outlier rows: ", boxplot.stats(rent$POPULATION)$out))

# Skewness function to examine normality of data
# install.packages("e1071")
# Density Plot
library(e1071)
# Divide graph area in 2 columns


par(mfrow = c(1, 2))


# Density plot for rents
plot(density(rent$X2018), main = "Density Plot :X2018",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(rent$X2018), 2)))

# Filling the area within the density plot to orange
polygon(density(rent$POPULATION), col = "orange")

# Density plot for population
plot(density(rent$POPULATION), main = "Density Plot :POPULATION",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(rent$POPULATION), 2)))

# Filling the area within the density plot to orange
polygon(density(rent$POPULATION), col = "orange")

# Calculating correlation test between rents in 2018 and population of 2018
cor(rent$X2018, rent$POPULATION)


# build linear regression model on full data
linearMod <- lm(POPULATION ~ X2018, data = rent)
linearMod

# model summary
summary(linearMod)

model_summary <- summary(linearMod)

# model coefficients
model_coeffs <- model_summary$coefficients
model_coeffs

# get beta estimate for rents in 2018
beta.estimate <- model_coeffs["X2018", "Estimate"]

# get std.error for rents
std_error <- model_coeffs["X2018", "Std. Error"]


# calc t statistic
t_value <- beta.estimate / std_error
p_value <- 2 * pt(-abs(t_value), df = nrow(rent) - ncol(rent)) # calc p Value
f_statistic <- linearMod$fstatistic[1] # fstatistic
f <- summary(linearMod)$fstatistic # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower = FALSE)


# sample chooses a random sample


no_of_records <- sample(1:nrow(rent), 0.8 * nrow(rent))
# model training data
training_data <- rent[no_of_records,]
training_data
# test data
testing_data <- rent[-no_of_records,]
testing_data


# Build the model on training data
# lm(formula, data) where
# formula describes the model to be fit
lm_model <- lm(X2018 ~ POPULATION, data = rent)

# model summary
summary(lm_model)

# predict rent from testing data
lm_predicted <- predict(lm_model, testing_data)
summary(lm_predicted)

# make actuals_predicteds dataframe.
lm_actuals_preds <- data.frame(cbind(actuals = testing_data$X2018, 
                                     predicted = lm_predicted))
head(lm_actuals_preds)

AIC(linearMod)

BIC(linearMod)

correlation_accuracy <- cor(lm_actuals_preds)
correlation_accuracy

# Min - max accuracy
lm_min_max_accuracy <- mean(apply(lm_actuals_preds, 1, min) / apply(lm_actuals_preds, 1, max))
lm_min_max_accuracy

# MAPE
lm_mape <- mean(abs((lm_actuals_preds$predicted - lm_actuals_preds$actuals)) / lm_actuals_preds$actuals)
lm_mape

# Global validation of linear model assumption
#install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(linearMod)
summary(gvmodel)


# Building the Polynomial model on training data
# lm(formula, data) where
# formula describes the model to be fit

poly_model <- lm(X2018 ~ POPULATION + I(POPULATION^2), data = rent)

# model summary
summary(poly_model)


# predicting from testing data
poly_predicted <- predict(poly_model, testing_data)
summary(poly_predicted)
# make actuals_predicteds dataframe.
poly_actuals_preds <- data.frame(cbind(actuals = testing_data$Rent, 
                                       predicted = poly_predicted))
head(poly_actuals_preds)

AIC(poly_model)

BIC(poly_model)

correlation_accuracy <- cor(poly_actuals_preds)
correlation_accuracy

# Min - max accuracy
poly_min_max_accuracy <- mean(apply(poly_actuals_preds, 1, min) / apply(poly_actuals_preds, 1, max))
poly_min_max_accuracy

# MAPE
poly_mape <- mean(abs((poly_actuals_preds$predicted - poly_actuals_preds$actuals)) / poly_actuals_preds$actuals)
poly_mape

summary(poly_predicted)

library(gvlma)
gvmodel <- gvlma(poly_model)
summary(gvmodel)

