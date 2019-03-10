library(lme4)
# library(lmerTest)
library(dplyr)
library(ggplot2)

# Set path and filename
path <- 'C:\\Users\\aleksander.molak\\Documents'
file <- 'countyBirthsDataUse.csv'

# Read-in the data
birthData <- read.csv(paste0(path, '.\\EDU\\DataCamp_-_MixedModels\\data', '\\', file))

# Build a model

# In this exercise, you'll build a hierarchical model with a global intercept (fixed-effect) 
# and random-effect for state. You will then look at the summary() of the model 
# and the plot() of the residuals. 

# Like other types of regression analysis, examining residuals can help you see 
# if anything is wrong with the model.

# With lmer(), there are two methods for doing this: 
# y ~ 1 + (1 | randomEffect) or the shortcut, y ~ (1 | randomEffect). 

# When building mixed-effect models, starting with simple models
# such as the global intercept model can check to see if problems exist with either the data or code.

birthRateStateModel <- lmer(BirthRate ~ (1|State), data=birthData)
summary(birthRateStateModel)
plot(birthRateStateModel)

# Predict
birthData$birthPredictState <-  predict(birthRateStateModel, birthData)

# Plot predicted data
ggplot() + 
  theme_minimal() +
  geom_point(data = birthData,
             aes(x = TotalPopulation, y = BirthRate)) + 
  geom_point(data = birthData,
             aes(x = TotalPopulation, y = birthPredictState),
             color = 'purple', alpha = 0.5) 

