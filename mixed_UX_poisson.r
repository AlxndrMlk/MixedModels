# Load lmerTest
library(lmerTest)

# Fit a Poisson glmer
glmerOut <- glmer(clicks ~ webpage + (1|group), data=userGroups, family='poisson')

# Examine output with summary()
summary(glmerOut)
