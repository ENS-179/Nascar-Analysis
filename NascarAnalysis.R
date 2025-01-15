library(ggplot2)
library(car)
library(MASS)
library(olsrr)

races <- read.csv("races.csv")
attach(races)

#########################################################################################################
#Exploratory Data analysis
summary(races)
names(races)

#Correlation Matrix of numeric variables
cor_matrix <- cor(races[, c("CHANGES", "DISTANCE", "CAUTIONS", "SPEED")]) # Only numeric columns
print("Correlation Matrix:")
print(cor_matrix)

#Checking normal distribution of CHANGES
qqnorm(races$CHANGES)

#Plots of each variable against CHANGES
plot(DISTANCE, CHANGES)
plot(CAUTIONS, CHANGES)
plot(SPEED, CHANGES)
plot(RESTRICTED, CHANGES)
plot(PLAYOFF, CHANGES)
plot(NEWGEN, CHANGES)

#Scatterplots of numeric variables
pairs(races[, c("CHANGES", "DISTANCE", "CAUTIONS", "SPEED")])

#Histograms numeric variables
hist(CHANGES, main = "Histogram of CHANGES", xlab = "CHANGES", ylab = "Frequency")
hist(DISTANCE, main = "Histogram of DISTANCE", xlab = "DISTANCE", ylab = "Frequency")
hist(SPEED, main = "Histogram of SPEED", xlab = "SPEED", ylab = "Frequency",)
hist(CAUTIONS, main = "Histogram of CAUTIONS", xlab = "CAUTIONS", ylab = "Frequency")

#Bar charts of categorical variables
ggplot(races, aes(factor(RESTRICTED))) + geom_bar() + labs(title="Bar Chart of RESTRICTED", x="Restricted", y="Count")
ggplot(races, aes(factor(PLAYOFF))) + geom_bar() + labs(title="Bar Chart of PLAYOFF", x="Playoff", y="Count")
ggplot(races, aes(factor(NEWGEN))) + geom_bar() + labs(title="Bar Chart of NEWGEN", x="New Generation", y="Count")

#Box plots of categorical variables vs CHANGES
ggplot(races, aes(factor(RESTRICTED), CHANGES)) + geom_boxplot() + labs(title="Boxplot of CHANGES by RESTRICTED", x="Restricted", y="CHANGES")
ggplot(races, aes(factor(PLAYOFF), CHANGES)) + geom_boxplot() + labs(title="Boxplot of CHANGES by PLAYOFF", x="Playoff", y="CHANGES")
ggplot(races, aes(factor(NEWGEN), CHANGES)) + geom_boxplot() + labs(title="Boxplot of CHANGES by NEWGEN", x="New Generation", y="CHANGES")

# Box plots of quantitative variables vs CHANGES, continuous variables binned
ggplot(races, aes(x = factor(CAUTIONS), y = CHANGES)) + geom_boxplot() + labs(title = "Boxplot of CHANGES by CAUTIONS", x = "CAUTIONS", y = "CHANGES")

races$DISTANCE_BIN <- cut(races$DISTANCE, breaks = 5, labels = c("Very Short", "Short", "Medium", "Long", "Very Long"))
ggplot(races, aes(x = DISTANCE_BIN, y = CHANGES)) + geom_boxplot() + labs(title = "Boxplot of CHANGES by Binned DISTANCE", x = "Binned DISTANCE", y = "CHANGES")

races$SPEED_BIN <- cut(races$SPEED, breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
ggplot(races, aes(x = SPEED_BIN, y = CHANGES)) + geom_boxplot() + labs(title = "Boxplot of CHANGES by Binned SPEED", x = "Binned SPEED", y = "CHANGES")

temp_model <- lm(CHANGES ~ DISTANCE + CAUTIONS + SPEED + I(log(SPEED)) +  RESTRICTED + PLAYOFF + NEWGEN, data = races)
crPlots(temp_model)


#########################################################################################################
# Maximum Model Selection and Diagnostics
summary(races)
names(races)
nrow(races)
ncol(races)

full_model <- lm(CHANGES ~ ., data=races)
full_model 
crPlots(full_model)

# Natural logarithmic transformation on SPEED
full_model_with_transformed<- lm(CHANGES ~ . + I(log(SPEED)), data = races)
full_model_with_transformed 
crPlots(full_model_with_transformed)

# Full model with interaction terms between DISTANCE and CAUTIONS and ln(SPEED) and RESTRICTED
full_model <- lm(CHANGES ~ DISTANCE * CAUTIONS + I(log(SPEED)) * RESTRICTED + I(log(SPEED)) + DISTANCE + CAUTIONS + SPEED + RESTRICTED + PLAYOFF + NEWGEN, data = races)
full_model 
plot(full_model)

plot(residuals(full_model), type = "o") 
abline(h = 0)

############## Assessing Assumptions ###########################################
#Normality appears violated
boxCox(full_model)
#Assess normality with shapiro test
shapiro.test(full_model$residuals)
#P-value is less than 0.05, normality violated.

plot(full_model$fitted.values, studres(full_model))
ols_test_breusch_pagan(full_model)
#Homoscedasticity also appears violated

######### Assessing Outliers ###################################################
#Leverage: Compare values to 2(k+1)/n. Outlier if greater
#k=8, n=164, 2(8+1)/164= 0.10976
tail(sort(hatvalues(full_model)), n=20)
#19 values appear to be outliers

#Cook's Distance: Compare values to 1
tail(sort(cooks.distance(full_model)), n=10) #Print 10 largest values
#No outliers according to Cook's distance

#Jackknife residuals follow t distribution with n-k-2 df.
#Compare to cutoff with upper 2.5%. 
t <- qt(.025, 164-8-2, lower.tail = FALSE)
print(t)
head(sort(studres(full_model)), n=20) # 45, 4, 79, 131, 155 appear to be outliers
tail(sort(studres(full_model)), n=20) # 159, 138, 16, 118, 136, 36 appear to be outliers

# Don't remove any outliers if plausible values

##################Fixing Assumptions############################################
#Try a square root transformation on y
full_model2 <- lm(sqrt(CHANGES) ~ DISTANCE * CAUTIONS + I(log(SPEED)) * RESTRICTED + I(log(SPEED)) + DISTANCE + CAUTIONS + SPEED + RESTRICTED + PLAYOFF + NEWGEN, data = races)
plot(full_model2)
shapiro.test(full_model2$residuals)
#p-value is now greater than 0.05, normality not violated anymore
plot(full_model2$fitted.values, studres(full_model2))
ols_test_breusch_pagan(full_model2)
#Homoscedasticity still appears violated

#Try a cubic root transformation
full_model3 <- lm((CHANGES)^(1/3) ~ DISTANCE * CAUTIONS + I(log(SPEED)) * RESTRICTED + I(log(SPEED)) + DISTANCE + CAUTIONS + SPEED + RESTRICTED + PLAYOFF + NEWGEN, data = races)
plot(full_model3)
shapiro.test(full_model3$residuals)
#p-value is greater than 0.05, normality not violated anymore
plot(full_model3$fitted.values, studres(full_model3))
ols_test_breusch_pagan(full_model3)
#Homoscedasticity still violated

boxCox(full_model) #Suggests a ^1/4 transformation
full_model4 <- lm((CHANGES)^(1/4) ~ DISTANCE * CAUTIONS + I(log(SPEED)) * RESTRICTED + I(log(SPEED)) + DISTANCE + CAUTIONS + SPEED + RESTRICTED + PLAYOFF + NEWGEN, data = races)
plot(full_model4)
shapiro.test(full_model4$residuals)
#p-value still greater than 0.05, normality not violated
plot(full_model4$fitted.values, studres(full_model4))
ols_test_breusch_pagan(full_model4)
#Homoscedasticity not violated anymore

############################################################
#Update full model to include this transformation
full_model <- lm((CHANGES)^(1/4) ~ DISTANCE * CAUTIONS + I(log(SPEED)) * RESTRICTED + I(log(SPEED)) + DISTANCE + CAUTIONS + SPEED + RESTRICTED + PLAYOFF + NEWGEN, data = races)

#########################################################################################################
#Final Model Selection
# Use backwards selection to pick best reduced model
reduced_model_backward_selection <- ols_step_backward_p(full_model, p_val = 0.10, details = TRUE)
print(reduced_model_backward_selection)

# Run diagnostics on final model
final_model <- lm((CHANGES)^(1/4) ~ I(log(SPEED)) + DISTANCE + SPEED + RESTRICTED, data = races)
ols_coll_diag(final_model)

# Collinearity violated so try standardizing main effect 
races$SPEED <- ((SPEED - mean(SPEED)) / sd(SPEED)) + (min(SPEED) + 1)
final_model <- lm((CHANGES)^(1/4) ~ I(log(SPEED)) + DISTANCE + SPEED + RESTRICTED, data = races)
ols_coll_diag(final_model)

# Collinearity still not fixed so remove transformation, revert main effect to original values, and rebuild model
races <- read.csv("races.csv")
final_model <- lm((CHANGES)^(1/4) ~ DISTANCE + SPEED + RESTRICTED, data = races)
# Check for collinearity
ols_coll_diag(final_model)
# Check for outliers
tail(sort(hatvalues(final_model)), n = 20)
tail(sort(cooks.distance(final_model)), n = 20)
print(qt(.025, 164 - 3 - 2, lower.tail = FALSE))
tail(sort(abs(studres(final_model))), n = 20)
# Check Independence, Linearity, Constant Variance, and Normality
plot(studres(final_model), type = "p")
plot(final_model$fitted.values, studres(final_model))
plot(final_model)
shapiro.test(final_model$residuals)
summary(final_model)
# Calculate MAE of final model
library(Metrics)
predictions <- predict(final_model, races)
mae((CHANGES)^(1/4), predictions)
