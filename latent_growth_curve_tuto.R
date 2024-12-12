# source : https://m-clark.github.io/sem/growth-curves.html
set.seed(1234)
n = 500
timepoints = 4
time = rep(0:3, times=n)
subject = rep(1:n, each=4)

intercept = .5
slope = .25
randomEffectsCorr = matrix(c(1,.2,.2,1), ncol=2) 

randomEffects = MASS::mvrnorm(n, mu=c(0,0), Sigma = randomEffectsCorr, empirical=T) 
randomEffects = data.frame(randomEffects)
colnames(randomEffects) = c('Int', 'Slope')

sigma = .5
y1 = (intercept + randomEffects$Int[subject]) + # random intercepts
  (slope + randomEffects$Slope[subject])*time + # random slopes 
  rnorm(n*timepoints, mean=0, sd=sigma)

d = data.frame(subject, time, y1)
head(d)

library(lme4)
mixedModel = lmer(y1 ~ time + (1 + time|subject), data=d)  # 1 represents the intercept
summary(mixedModel)

library(dplyr)
library(tidyr)

dWide <- spread(d, time, y1)

new_names <- names(dWide)
new_names <- ifelse(new_names != "subject", paste0("y", new_names), new_names)
names(dWide) <- new_names

head(dWide)

library(lavaan)
model = "
    # intercept and slope with fixed coefficients
    i =~ 1*y0 + 1*y1 + 1*y2 + 1*y3
    s =~ 0*y0 + 1*y1 + 2*y2 + 3*y3
"
growthCurveModel = growth(model, data=dWide)
summary(growthCurveModel)

fixef(mixedModel)
lm(y1 ~ time, data=d)

VarCorr(mixedModel)
