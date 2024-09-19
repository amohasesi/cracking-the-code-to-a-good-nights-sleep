# Data Import and Cleaning
sleep <- read.csv("sleep.csv", header = TRUE)

sleep <- sleep[, -c(1, 4, 5)]

sleep$Gender <- ifelse(sleep$Gender == "Male", 1, 0) 
sleep$Smoking.status <- ifelse(sleep$Smoking.status == "Yes", 1, 0) 

for (i in 1 : ncol(sleep)) {
  sleep[is.na(sleep[ , i]), i] <- median(sleep[ , i], na.rm = TRUE)
}

temp_sleep <- sleep
temp_sleep[temp_sleep == 0] <- 0.0001


# Exploratory Data Analysis
pairs(sleep)

sapply(sleep, mean)
sapply(sleep, median)
sapply(sleep, sd)

cor(sleep)
cor(sleep$Age, sleep$Sleep.efficiency)
cor(sleep$Gender, sleep$Sleep.efficiency)
cor(sleep$Sleep.duration, sleep$Sleep.efficiency)
cor(sleep$REM.sleep.percentage, sleep$Sleep.efficiency)
cor(sleep$Deep.sleep.percentage, sleep$Sleep.efficiency)
cor(sleep$Light.sleep.percentage, sleep$Sleep.efficiency)
cor(sleep$Awakenings, sleep$Sleep.efficiency)
cor(sleep$Caffeine.consumption, sleep$Sleep.efficiency)
cor(sleep$Alcohol.consumption, sleep$Sleep.efficiency)
cor(sleep$Smoking.status, sleep$Sleep.efficiency)
cor(sleep$Exercise.frequency, sleep$Sleep.efficiency)

par(mfrow = c(3, 2))
hist(sleep$Age)
hist(sleep$Gender)
hist(sleep$Sleep.duration)
hist(sleep$REM.sleep.percentage)
hist(sleep$Deep.sleep.percentage)
hist(sleep$Light.sleep.percentage)

par(mfrow = c(3, 2))
hist(sleep$Awakenings)
hist(sleep$Caffeine.consumption)
hist(sleep$Alcohol.consumption)
hist(sleep$Smoking.status)
hist(sleep$Exercise.frequency)
hist(sleep$Sleep.efficiency)

library(ggplot2)
require(gridExtra)

g1 <- ggplot(sleep, aes(Age, Sleep.efficiency, color = Gender)) +  geom_point(size = 2)
g2 <- ggplot(sleep, aes(REM.sleep.percentage, Sleep.efficiency, color = Gender)) +  geom_point(size = 2)
g3 <- ggplot(sleep, aes(Deep.sleep.percentage, Sleep.efficiency, color = Gender)) +  geom_point(size = 2)
g4 <- ggplot(sleep, aes(Light.sleep.percentage, Sleep.efficiency, color = Gender)) +  geom_point(size = 2)
g5 <- ggplot(sleep, aes(Awakenings, Sleep.efficiency, color = Gender)) +  geom_point(size = 2)
g6 <- ggplot(sleep, aes(Caffeine.consumption, Sleep.efficiency, color = Gender)) +  geom_point(size = 2)
g7 <- ggplot(sleep, aes(Alcohol.consumption, Sleep.efficiency, color = Gender)) +  geom_point(size = 2)
g8 <- ggplot(sleep, aes(Exercise.frequency, Sleep.efficiency, color = Gender)) +  geom_point(size = 2)
grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, ncol = 2)

s1 <- ggplot(sleep, aes(Age, Sleep.efficiency, color = Smoking.status)) +  geom_point(size = 2)
s2 <- ggplot(sleep, aes(REM.sleep.percentage, Sleep.efficiency, color = Smoking.status)) +  geom_point(size = 2)
s3 <- ggplot(sleep, aes(Deep.sleep.percentage, Sleep.efficiency, color = Smoking.status)) +  geom_point(size = 2)
s4 <- ggplot(sleep, aes(Light.sleep.percentage, Sleep.efficiency, color = Smoking.status)) +  geom_point(size = 2)
s5 <- ggplot(sleep, aes(Awakenings, Sleep.efficiency, color = Smoking.status)) +  geom_point(size = 2)
s6 <- ggplot(sleep, aes(Caffeine.consumption, Sleep.efficiency, color = Smoking.status)) +  geom_point(size = 2)
s7 <- ggplot(sleep, aes(Alcohol.consumption, Sleep.efficiency, color = Smoking.status)) +  geom_point(size = 2)
s8 <- ggplot(sleep, aes(Exercise.frequency, Sleep.efficiency, color = Smoking.status)) +  geom_point(size = 2)
grid.arrange(s1, s2, s3, s4, s5, s6, s7, s8, ncol = 2)


# Model 1: Full Model Using the Original Data
lm_sleep <- lm(sleep$Sleep.efficiency ~ sleep$Age + sleep$Gender + sleep$Sleep.duration + sleep$REM.sleep.percentage + sleep$Deep.sleep.percentage +
                 sleep$Light.sleep.percentage + sleep$Awakenings + sleep$Caffeine.consumption + sleep$Alcohol.consumption + sleep$Smoking.status + 
                 sleep$Exercise.frequency)
summary(lm_sleep)

par(mfrow = c(2, 2))
plot(lm_sleep)

par(mfrow = c(2, 3))
plot(sleep$Age, rstandard(lm_sleep), ylab='Standardized Residuals', xlab='Age') 
plot(sleep$Gender, rstandard(lm_sleep), ylab='Standardized Residuals', xlab='Gender') 
plot(sleep$Sleep.duration, rstandard(lm_sleep), ylab='Standardized Residuals', xlab='Sleep Duration') 
plot(sleep$REM.sleep.percentage, rstandard(lm_sleep), ylab='Standardized Residuals', xlab='REM Sleep Percentage') 
plot(sleep$Deep.sleep.percentage, rstandard(lm_sleep), ylab='Standardized Residuals', xlab='Deep Sleep Percentage') 
plot(sleep$Light.sleep.percentage, rstandard(lm_sleep), ylab='Standardized Residuals', xlab='Light Sleep Percentage') 
plot(sleep$Awakenings, rstandard(lm_sleep), ylab='Standardized Residuals', xlab='Awakenings') 
plot(sleep$Caffeine.consumption, rstandard(lm_sleep), ylab='Standardized Residuals', xlab='Caffeine Consumption') 
plot(sleep$Alcohol.consumption, rstandard(lm_sleep), ylab='Standardized Residuals', xlab='Alcohol Consumption') 
plot(sleep$Smoking.status, rstandard(lm_sleep), ylab='Standardized Residuals', xlab='Smoking Status') 
plot(sleep$Exercise.frequency, rstandard(lm_sleep), ylab='Standardized Residuals', xlab='Exercise Frequency') 


# Model 2: Transformation of the Response Using an Inverse Response Plot
library(car)
inverseResponsePlot(lm_sleep) 

tSleep.efficiency <- log(sleep$Sleep.efficiency)
lm_sleep_inverse <- lm(tSleep.efficiency ~ sleep$Age + sleep$Gender + sleep$Sleep.duration + sleep$REM.sleep.percentage + sleep$Deep.sleep.percentage +
                         sleep$Light.sleep.percentage + sleep$Awakenings + sleep$Caffeine.consumption + sleep$Alcohol.consumption + sleep$Smoking.status + 
                         sleep$Exercise.frequency)

summary(lm_sleep_inverse)
par(mfrow = c(2, 2))
plot(lm_sleep_inverse)


# Model 3: Transformation of the Predictors Using the Box-Cox Method and Transformation of the Response Using an Inverse Response Plot
summary(powerTransform(cbind(temp_sleep$Age, temp_sleep$Gender, temp_sleep$Sleep.duration, temp_sleep$REM.sleep.percentage, temp_sleep$Deep.sleep.percentage, temp_sleep$Awakenings, temp_sleep$Caffeine.consumption, temp_sleep$Alcohol.consumption, temp_sleep$Smoking.status, temp_sleep$Exercise.frequency) ~ 1))

tGender <- log(temp_sleep$Gender)
tDeep.sleep.percentage <- (temp_sleep$Deep.sleep.percentage)^(2.47)
tAwakenings <- (temp_sleep$Awakenings)^(0.33)
tCaffeine.consumption <- log(temp_sleep$Caffeine.consumption)
tAlcohol.consumption <- log(temp_sleep$Alcohol.consumption)
tSmoking.status <- (temp_sleep$Smoking.status)^(-0.22)
tExercise.frequency <- (temp_sleep$Exercise.frequency)^(0.29)

lm_sleep_power <- lm(sleep$Sleep.efficiency ~ sleep$Age + tGender + sleep$Sleep.duration + sleep$REM.sleep.percentage + tDeep.sleep.percentage 
                     + sleep$Light.sleep.percentage + tAwakenings + tCaffeine.consumption + tAlcohol.consumption 
                     + tSmoking.status + tExercise.frequency)

inverseResponsePlot(lm_sleep_power)
tSleep.efficiency <- log(temp_sleep$Sleep.efficiency)

lm_sleep_power2 <- lm(tSleep.efficiency ~ sleep$Age + tGender + sleep$Sleep.duration + sleep$REM.sleep.percentage + tDeep.sleep.percentage 
                      + sleep$Light.sleep.percentage + tAwakenings + tCaffeine.consumption + tAlcohol.consumption 
                      + tSmoking.status + tExercise.frequency)

summary(lm_sleep_power2)
par(mfrow = c(2, 2))
plot(lm_sleep_power)

library(leaps)
vif(lm_sleep_power2)


# Forward Stepwise Variable Selection Applied to Model 3
mint <- lm(tSleep.efficiency ~ 1)
forwardAIC <- step(mint, scope = list(lower = ~1, upper = ~sleep$Age + tGender + sleep$Sleep.duration + sleep$REM.sleep.percentage + tDeep.sleep.percentage 
                                      + sleep$Light.sleep.percentage + tAwakenings + tCaffeine.consumption + tAlcohol.consumption 
                                      + tSmoking.status + tExercise.frequency, direction = "forward"))


# Backwards Stepwise Variable Selection Applied to Model 3 
backwardAIC <- step(lm_sleep_power, direction = "backward")


# Model 4: Model 3 Post Variable Selection
lm_sleep_final <- lm(tSleep.efficiency ~ sleep$Light.sleep.percentage + tAwakenings + 
                       tSmoking.status + sleep$Age + tAlcohol.consumption + tDeep.sleep.percentage + 
                       tExercise.frequency)

summary(lm_sleep_final)
par(mfrow = c(2, 2))
plot(lm_sleep_final)

library(car)
vif(lm_sleep_final)

n <- nrow(sleep)
AIC <- extractAIC(lm_sleep_final)[2]
AICc <- extractAIC(lm_sleep_final)[2] + 2 * (7 + 2) * (7 + 3) / (n - 7 - 1)
BIC <- extractAIC(lm_sleep_final, k = log(n))[2]

AIC
AICc
BIC

library(car)
par(mfrow = c(2, 2))
avPlot(lm_sleep_final, variable = sleep$Light.sleep.percentage, ask = FALSE)
avPlot(lm_sleep_final, variable = tAwakenings, ask = FALSE)
avPlot(lm_sleep_final, variable = tSmoking.status, ask = FALSE)
avPlot(lm_sleep_final, variable = sleep$Age, ask = FALSE)

par(mfrow = c(2, 2))
avPlot(lm_sleep_final, variable = tAlcohol.consumption, ask = FALSE)
avPlot(lm_sleep_final, variable = tDeep.sleep.percentage, ask = FALSE)
avPlot(lm_sleep_final, variable = tExercise.frequency, ask = FALSE)