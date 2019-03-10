library(lme4)
# library(lmerTest)
library(dplyr)
library(ggplot2)
library(broom)

# Set path and filename
path <- 'C:\\Users\\aleksander.molak\\Documents'
file <- 'countyBirthsDataUse.csv'

# Read-in the data
birthData <- read.csv(paste0(path, '.\\EDU\\DataCamp_-_MixedModels\\data', '\\', file))

# Build a hierarchical model with countyBirthsData, which has been loaded for you, and include a fixed-effect. 
# Does the average age of the mother at birth predict the birth rate?

# Include the AverageAgeofMother as a fixed effect within the lmer and state as a random effect
ageMotherModel <- lmer(BirthRate ~ AverageAgeofMother + (1|State),
                       birthData)
summary(ageMotherModel)


# Create a new feat log10(TotalPop)
birthData$LogTotalPop <- log10(birthData$TotalPopulation)

#----------------------------------------------------

# Estimate a random-effect slope for each state. For example, 
# perhaps the log10(total population of each county), 
# LogTotalPop, changes the birth rate of a county AND varies by state. 

# Include the AverageAgeofMother as fixed-effect and State as a random-effect
model_A <- lmer( BirthRate ~ AverageAgeofMother + (1|State), birthData)
summary(model_A)

# Include the AverageAgeofMother as fixed-effect and LogTotalPop and State as random-effects
model_B <- lmer( BirthRate ~ AverageAgeofMother + (LogTotalPop|State), birthData)
summary(model_B)

# Uncorrelated random-effect slope
# In the previous exercise, you use lme4's' default setting and assumed slopes 
# and intercepts within each group were correlated for the random-effect estimates. 
# However, this assumption may not always be valid or we may want to simplify the model 
# if we are having trouble numerically fitting the model.

# Building a model with uncorrelated random-effects is one method for 
# potentially simplifying the model. 
# Furthermore, lmer() models can be hard to fit and checking model outputs can be 
# a useful step when debugging your model. 
# Alternatively, you may have subject matter expertise and want to assume 
# the random-effects are not correlated.
# 
# To fit a model with an uncorrelated random-effect slope, use || rather than | with lmer() syntax. 

# Include AverageAgeofMother as fixed-effect and LogTotalPop and State as uncorrelated random-effects
model_C <- lmer( BirthRate ~ AverageAgeofMother + (LogTotalPop||State),
                 birthData)

# Compare outputs of both models 
summary(model_B)
summary(model_C)

# --------------------------------

# Sometimes, a model can have the same predictor as both a fixed and random-effect. 
# For example, perhaps you are interested in estimating the average effect the age of a mother at birth 
# (AverageAgeofMother). 
# 
# Including the predictor as fixed-effect allows you to estimate the effect 
# of a mother's age across all locations. 
# 
# Including the predictor as a random-effect allows you to simultaneously 
# account (or correct) for different slope estimates among states.

#----------------------------------

# Construct a lmer() using AverageAgeofMother as a fixed-effect and AverageAgeofMother 
# as a random-effect nested within State to predict BirthRate with the countyBirthsData. 

#Make sure the fixed-effect goes before the random-effects in the formula.

out <- lmer(BirthRate ~ AverageAgeofMother + (AverageAgeofMother|State), data=birthData)

summary(out)

fixef(out) # get info about fixed effects only
ranef(out) # get info about rndom effects only

confint(out)


# -----------------------------------
# Plot the results

# Extract out the parameter estimates and confidence intervals and manipulate the data
dataPlot <- data.frame(cbind( fixef(out), confint(out)[ 5:6, ]))
rownames(dataPlot)[1] <- "Intercept"
colnames(dataPlot) <- c("est", "L95", "U95")
dataPlot$parameter <- rownames(dataPlot)

# Print the new dataframe
print(dataPlot)

# Plot the results using ggplot2
ggplot(dataPlot, aes(x = parameter, y = est,
                     ymin = L95, ymax = U95)) +
  geom_hline( yintercept = 0, color = 'red', alpha=.5 ) +
  geom_linerange() + geom_point() + coord_flip() + theme_minimal()
