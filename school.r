# Mixed effect model with 
# `sex`, `mathprep` and `mathknow` as fixed 
# and `classid` and `schoolid` as random effects

lmerModel <- lmer(mathgain ~ 
                  sex + mathprep + mathknow + 
                  (1|classid) + (1|schoolid), 
                  data = studenData, na.action = "na.omit",
                  REML = TRUE)
summary(lmerModel)
