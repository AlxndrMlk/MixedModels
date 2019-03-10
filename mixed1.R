# MIXED MODELS @ DataCamp.com:
# https://campus.datacamp.com/courses/hierarchical-and-mixed-effects-models

library(ggplot2)
library(broom)
library(lme4)
library(lmerTest)

# Ex. 1
# During these exercises, you will plot and model how math test score gains vary as a function 
# of socioeconomic status across three levels without accounting for the hierarchical nature of the data.
# In this exercise, visualize and model the data at student-level. 
# In the next exercise, you will explore other levels.
path <- 'C:\\Users\\aleksander.molak\\Documents'
# Read-in the data
studentData <- read.csv(paste0(path, '.\\EDU\\DataCamp_-_MixedModels\\data\\classroom.csv'))

# Plot the data
ggplot(data = studentData, aes(x = housepov, y = mathgain)) +
  geom_point(color='dark blue', alpha=.3) +
  geom_smooth(method = 'lm', color='red')

# Fit a linear model
summary(lm(mathgain ~ housepov , data =  studentData))

# NOTE: This model explains no variance at all!

# Fit the real model now :)
# Mixed effect model
lmerModel <- lmer(mathgain ~ 
                    sex + mathprep + mathknow + 
                    (1|classid) + (1|schoolid), 
                  data = studentData, na.action = "na.omit",
                  REML = TRUE)
summary(lmerModel)
# ------------------------------------------------


