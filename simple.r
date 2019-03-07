library(lme4)

# Run model
outLmer <- lmer(response ~ x + (1|group), data = multIntDemo)

# Look at model outputs 
summary(outLmer)
# tidy(outLmer)
