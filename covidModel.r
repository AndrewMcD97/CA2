# Reading In The Covid World Progression .csv Into
# A Dataframe
covidProgression <- read.csv("covid.csv", na="")

# Checking The Structure Of Our Dataset
summary(covidProgression)
str(covidProgression)

# Make the dataframe smaller with only the varibales needed to
# answer the question posed
deathResearch <- subset(covidProgression, select = c(4,14,46,48,49,51,52,53,54,55))

# Variables are: hosp_patients_per_million , population_density,
# aged_65_older,aged_70_older,extreme_poverty, 
# cardiovasc_death_rate ,diabetes_prevalence ,female_smokers ,male_smokers 

# Check Variable Types
str(deathResearch)

# We need to convert date from chr to Date
deathResearch$date <- as.Date(deathResearch$date, "%Y-%m-%d")

# We only want the latest date available to us which is 27/04/21
deathResearch <- deathResearch[deathResearch$date == "2021-04-27", ]

deathResearch <- na.omit(deathResearch) 


# The model will be an MLR 

# We must check the model assumptions
# -----------------------------
# check outliers 
# ------------------------------

# Check for outliers
opar <- par(no.readonly = TRUE)
attach(deathResearch)
head(deathResearch)
par(mfrow = c(3, 2)) # charts shown in 4 rows x 2 cols

boxplot(total_deaths_per_million, 
        main = "Deaths Per Million", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(total_deaths_per_million)$out))
cars <- subset(cars, cars$dist!=120)

boxplot(aged_65_older, 
        main = "Aged 65 Or Older", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(aged_65_older)$out))

boxplot(aged_70_older, 
        main = "Aged 70 Or Older", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(aged_70_older)$out))

boxplot(extreme_poverty, 
        main = "Extreme Poverty", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(extreme_poverty)$out))

boxplot(cardiovasc_death_rate, 
        main = "Cardio Death Rate %", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(cardiovasc_death_rate)$out))

boxplot(female_smokers, 
        main = "Female smokers", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(female_smokers)$out))


par(opar)

# Delete Outliers
deathResearch <- subset(deathResearch, deathResearch$total_deaths_per_million!=2793.274)
deathResearch <- subset(deathResearch, deathResearch$female_smokers!=44)




# Multiple Linear regression (MLR)

# Linearity: There is a linear relation among the variables
# Normality: Residuals are normally distributed
# Homoscedasticity: Residuals have constant variance
# No collinearity: Variables are not linear combinations of each other
# Independence: Residuals are independent or at least not correlated 

#-----------------------------------------------------------------
# Linearity
#-------------------------------------------------------------------
# Try the pairs() function between each variable and hospitalization.

pairs(deathResearch)

install.packages("psych")
library(psych)
pairs.panels(deathResearch, 
             smooth = TRUE, # If TRUE, draws loess smooths  
             scale = FALSE, # If TRUE, scales the correlation text font  
             density = TRUE, # If TRUE, adds density plots and histograms  
             ellipses = TRUE, # If TRUE, draws ellipses   
             method = "spearman",# Correlation method (also "pearson" or "kendall") 
             pch = 21, # pch symbol   
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit 
             cor = TRUE, # If TRUE, reports correlations
             jiggle = FALSE, # If TRUE, data points are jittered  
             factor = 2, # Jittering factor  
             hist.col = 4, # Histograms color   
             stars = TRUE,
             ci = TRUE) # If TRUE, adds confidence intervals 

attach(deathResearch)
# Check correlation of variables to check for linearity

opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))
scatter.smooth(x=total_deaths_per_million,
               y=aged_65_older,
               main="total_deaths_per_million ~ aged_65_older",
               xlab="total_deaths_per_million",
               ylab="Over Age 65's %")

scatter.smooth(x=total_deaths_per_million,
               y=aged_70_older,
               main="Insurance total_deaths_per_million ~ aged_70_older",
               xlab="total_deaths_per_million",
               ylab="Over Age 70's %")

scatter.smooth(x=total_deaths_per_million,
               y=extreme_poverty,
               main="Insurance total_deaths_per_million ~ Poverty %",
               xlab="total_deaths_per_million",
               ylab="Poverty")

scatter.smooth(x=total_deaths_per_million,
               y=cardiovasc_death_rate,
               main="Insurance total_deaths_per_million ~ cardiovasuclar death rate %",
               xlab="total_deaths_per_million",
               ylab="death rate %")

scatter.smooth(x=total_deaths_per_million,
               y=female_smokers,
               main="Insurance total_deaths_per_million ~ Female Smokers",
               xlab="total_deaths_per_million",
               ylab="Female Smokers")



# Values of -0.2 < x < 0.2 - low correlation
paste("correltion for total_deaths_per_million and female_smokers: ", cor(total_deaths_per_million, aged_65_older, use = "complete.obs"))
paste("correltion for total_deaths_per_million and extreme_poverty: ", cor(total_deaths_per_million, aged_70_older, use = "complete.obs"))
paste("correltion for total_deaths_per_million and Population density: ", cor(total_deaths_per_million, population_density, use = "complete.obs"))
paste("correltion for total_deaths_per_million and poverty: ", cor(total_deaths_per_million, extreme_poverty, use = "complete.obs"))
paste("correltion for total_deaths_per_million and cardiovasc_death_rate: ", cor(total_deaths_per_million, cardiovasc_death_rate, use = "complete.obs"))
paste("correltion for total_deaths_per_million and diabetes: ", cor(total_deaths_per_million, diabetes_prevalence, use = "complete.obs"))
paste("correltion for total_deaths_per_million and female smokers: ", cor(total_deaths_per_million, female_smokers, use = "complete.obs"))
paste("correltion for total_deaths_per_million and male smokers: ", cor(total_deaths_per_million, male_smokers, use = "complete.obs"))

# Going to remove male_smokers, diabetes, population_density as it seems to have little correlation to the death rate.
deathResearch <- subset(deathResearch, select = -c(male_smokers,diabetes_prevalence,population_density))
head(deathResearch)

#-----------------------------------------------------------------
# Normality
#-------------------------------------------------------------------

# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical

# Check normality
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))
plot(density(total_deaths_per_million), 
     main = "Density plot : Total Deaths Per Mil", 
     ylab = "Frequency", xlab = "Deaths",
     sub = paste("Skewness : ", round(e1071::skewness(total_deaths_per_million), 2)))
polygon(density(total_deaths_per_million), col = "red")


plot(density(aged_65_older), 
     main = "Density plot : Over 65's", 
     ylab = "Frequency", xlab = "Over 65's %",
     sub = paste("Skewness : ", round(e1071::skewness(aged_65_older), 2)))
polygon(density(aged_65_older), col = "red")

plot(density(aged_70_older), 
     main = "Density plot : Over 70's", 
     ylab = "Frequency", xlab = "Over 70's %",
     sub = paste("Skewness : ", round(e1071::skewness(aged_70_older), 2)))
polygon(density(aged_70_older), col = "red")

plot(density(extreme_poverty), 
     main = "Density plot : extreme_poverty", 
     ylab = "Frequency", xlab = "extreme_poverty",
     sub = paste("Skewness : ", round(e1071::skewness(extreme_poverty), 2)))
polygon(density(extreme_poverty), col = "red")

plot(density(cardiovasc_death_rate), 
     main = "Density plot : cardiovasc_death_rate", 
     ylab = "Frequency", xlab = "cardiovasc_death_rate",
     sub = paste("Skewness : ", round(e1071::skewness(cardiovasc_death_rate), 2)))
polygon(density(cardiovasc_death_rate), col = "red")

plot(density(female_smokers), 
     main = "Density plot : female_smokers", 
     ylab = "Frequency", xlab = "Female Smoker %",
     sub = paste("Skewness : ", round(e1071::skewness(female_smokers), 2)))
polygon(density(female_smokers), col = "red")

par <- opar



# ------------------
# Fitting the model
# -------------------

set.seed(1)
no_rows_data <- nrow(deathResearch)

sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace =FALSE)
training_data <- deathResearch[sample, ]
testing_data <- deathResearch[-sample, ]

fit <- lm(total_deaths_per_million ~ aged_65_older +
                  aged_70_older + extreme_poverty + 
                  cardiovasc_death_rate + female_smokers, data=training_data)

summary(fit)

install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)

#-------------
# Forcasting
# -------------------
# predict distance from testing data
deathDistance <- predict(fit, testing_data)

# This prediction will then be used against the testing_datas total_deaths
# variable in order to see how accurate the model is
actualPrediction <- data.frame(cbind(actuals = testing_data$total_deaths_per_million, 
                                     predicted = deathDistance))


head(actualPrediction)


correlation_accuracy <- cor(actualPrediction)
correlation_accuracy



# This is just one prediction
# We will now see how accurate or inaccurate the predicted values are
# versus the actual values
# Min - max accuracy
min_max_accuracy <- mean(apply(actualPrediction, 1, min) / apply(actualPrediction, 1, max))
min_max_accuracy