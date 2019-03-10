library(lme4)
# library(lmerTest)
library(dplyr)
library(ggplot2)
library(broom)

# Set path and filename
path <- 'C:\\Users\\aleksander.molak\\Documents'
file <- 'MDcrime.csv'

# Read-in the data
crime <- read.csv(paste0(path, '.\\EDU\\DataCamp_-_MixedModels\\data', '\\', file))

# VISUALIZE TO EXAMINE RANDOM EFFECTS
# To explore this data, first plot the data points for each county through time. 
# This lets you see how each county changes through time. 
# Rather than using an aesthetic such as color, group is used here 
# because there are too many counties to easily distinguish colors. 
# After plotting the raw data, add trend lines for each county.
# 
# Both the connect points (geom_line) and trend lines (geom_smooth) provide insight into what, 
# if any, kinds of random effects are required. 
#
# If all of the points appear to have similar ranges and means, 
# a random-effect intercept may not be important. 
# Likewise, if trends look consistent across counties 
# (i.e., the trend lines look similar or parallel across groups), 
# a random-effect slope may not be required.

# Plot the change in crime through time by County
plot1 <- ggplot(data = crime, aes(x = Year, y = Crime, group = County)) +
  geom_line(alpha=.3) + 
  theme_minimal() +
  ylab("Major crimes reported per county")
print(plot1)

# Add the trend line for each county
plot1 + geom_smooth(method='lm', se=FALSE, alpha=.2)


# Build a lmer() to predict Crime with Year as both a fixed-effect and random-effect slope 
# and County as the random-effect intercept. 
# Fit the model with Year2 rather than Year - RESCALING FOR NUMERICAL STABILITY
lmer(Crime ~ Year2 + (1 + Year2 | County) , data = MDCrime)

