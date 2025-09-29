# Growth curve analysis
# a dummy example of (not latent) growth curve analysis
# based on:
# https://stats.stackexchange.com/questions/354732/latent-growth-curve-model-with-more-time-points-than-participants
library(lme4) # for mixed (or multilevel) model
library(lattice) # the plot library that goes hand-in-hand with lme4

set.seed(1234)
n_study <- 12 # number of study
n_time <- 500 # number of time points per study (the same here, for simplicity)

slope <- 1  # average slope
sd_slope <- 1 # sd of the slopes (between people)
sd_resid <- 1 # sd of the resids

# generate slopes
slopes <- rnorm(n_study, 1, 1)

d <- lapply(slopes, function(x) {
  time <- 1:n_time  # time variable
  # generate data (angle) for each study- their slope + residual
  angle <-   time * x + rnorm(n_time, mean = 0, sd = sd_resid)
  return(data.frame(time = time, angle = angle))
})

# put i for individual into each data frame
for(i in 1:length(d)) {
  d[[i]]$study <- i
}

# combine data frames in d
dLong <- do.call(rbind, d)

# add a variable "vivo" (with 3 ex vivo studies vs. 9 in vivo studies)
dLong$vivo = ifelse(dLong$study < 4, "ex", "in")
# recode it as -0.5 vs 0.5
dLong$vivo_c = ifelse(dLong$study < 4, -0.5, 0.5)

# print first rows
head(dLong, n = 10)
str(dLong)

# plot data
plot(x = dLong$time, y = dLong$angle,
     type = "l",
     xlab = "Time",
     ylab = "Angle",
     col = "grey")
points(x = dLong$time,
       y = dLong$angle,
       col =  as.integer(as.factor(dLong$vivo)) |> adjustcolor(.5))
legend("topleft", legend = c("ex-vivo", "in-vivo"),
       pch = 1, col = c(1,2), lwd = 2)

# fit a dumb model assuming no effect of time
# vivo is a between-study factor
# (1 | study) assuming we fit one intercept by study
# 1 is the intercept
mod0 <- lmer(angle ~ 1 + vivo + (1|study), data = dLong)

# fit growth curve to all data
# NB: this tests the linear effect of time
# We have added the time to check the effect of time on in vivo
# angle, regresser sur le predicteur time vivo + (1|study)
mod1 <- lmer(angle ~ time + vivo + (1|study), data = dLong)

# fit another model without the variable vivo
mod2 <- lmer(angle ~ time + (1|study), data = dLong)

# fit another model without the variable vivo (Pierre)
mod3 <- lmer(angle ~ poly(time,2) + (1|study), data = dLong)

# test the effect of time:
# it does not do an anova wesh! 
# this is much more ratio test
anova(mod0, mod1)
# very significant of course

# test the effect of vivo:
anova(mod1, mod2)
# the test here is not significant

# test the effect of vivo:
anova(mod2, mod3)

# code showing intercept and slopes by study for mod1: 
lattice::qqmath(ranef(mod1))


# code showing intercept and slopes by study for mod1: 
lattice::qqmath(ranef(mod3))
lattice::qqmath(resid(mod3))
lattice::histogram(resid(mod3))
# future work: testing more complex angle ~ time relationships!
