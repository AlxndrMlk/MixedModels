library(lme4)

# Fit a model with one fixed (`x`) and one random (`group`) effect (intercept only)
# Note the specific syntax (`(1|<your_var>)`) for random effect
outLmer <- lmer(response ~ x + (1|group), data = multIntDemo)

# Look at model outputs 
summary(outLmer)
# tidy(outLmer)

# Random-effect slopes:
outLmer2 <- lmer(response ~ (x|group), multIntDemo)

summary(outLmer2)
#tidy(outLmer2)
