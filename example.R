library(devtools)
devtools::install_github("georgieevans/PrivacyUnbiased")
library(PrivacyUnbiased)

# load the private data
data("private_data")

# load the dp data
data('dp_data')

lmdp_test <- lmdp(Y ~ X1 + X2 + X3, data = dp_data)

summary(lmdp_test)

# this summarizes output of lmdp object
str(lmdp_test)
lmdp_test$beta_tilde_vcov

# comparing to lm not adjusted for random error
lm_test <- lm(Y ~ X1 + X2 + X3, data = dp_data)

# biased OLS estimates
round(summary(lm_test)$coef, 4)

# true lm on private data
lm_true <- lm(Y ~ Z1 + Z2 + Z3, data = private_data)
round(summary(lm_true)$coef, 4)

## Estimating variance using bootstrap!
# lmdp uses simulation to estimate

# timing simulation variance estimation
system.time(simulation <- lmdp(Y ~ X1 + X2 + X3, data = dp_data))

# timing bootstrap variance estimation
system.time(bootstrap <- lmdp(Y ~ X1 + X2 + X3, data = dp_data, bootstrap_var = TRUE))

# standard error comparisons:
summary(bootstrap)[, "Std. Error"]
summary(simulation)[, "Std. Error"]

lmdp_test$vc_pos_def

# variable transformation: interaction and squared

# interaction variable
lmdp_interaction <- lmdp(Y ~ X1 + X2 + X3 + X1*X2, data = dp_data)
summary(lmdp_interaction)

# interactions produce similar estimates b/w lm, lmdp
lm_interaction <- lm(Y ~ Z1 + Z2 + Z3 + Z1*Z2, data = private_data)
round(summary(lm_interaction)$coef, 4)

# descriptive statistics estimates on PRIVATE data from dp data

# main function is descriptiveDP()
descriptiveDP(X3, dp_data)

# compare estimates to true values from private data
true_descriptive <- round(c(mean(private_data$Z3),
                            sd(private_data$Z3),
                            moments::skewness(private_data$Z3),
                            moments::kurtosis(private_data$Z3)), 4)

names(true_descriptive) <- c('Mean', 'Std.Dev', 'Skewness', 'Kurtosis')

true_descriptive

# ----------------
# load the datasets
data('private_data2')
data('dp_data2')

# estimating private histograms, use distributionDP()

dist_test <- distributionDP(variable = X1, data = dp_data2,
                            distributions = c('Normal','Poisson',
                                              'ZIP', 'NB', 'ZINB'),
                            moments_fit = 6, plot = TRUE, plot_dp = TRUE)

# check moments were estimated with sufficient precision
dist_test$Normal$moment_precision

# ZINB ratios are all close to 1, so it fits data well
# check with plots (cheating by using private data)
dist_test$ZINB$plot

# by comparison...
dist_test$NB$plot

# regression diagnostics

# diagnosticsDP() runs regression diagnostics that would be run
# after OLS on private data

# first, run bias corrected regression
lmdp_obj <- lmdp(Y ~ X1, data = dp_data2)

# now run regression diagnostics
diagnostics <- diagnosticsDP(lmdp_obj)

# the above shows no strong evidence of non-normality
# we also detect heteroskedasticity since X1 coeff is far from 0

# to valide, we can check results against private data
# calculate true errors
true_coef <- c(2,8)
true_errors <- private_data2$Y - cbind(1, private_data2$Z1)%*%true_coef

# validate they are approx normally distr
# 1. skewness is 0
moments:: skewness(true_errors)

# 2. kurtosis is 3
moments::kurtosis(true_errors)

# 3. test heteroskedasticity
summary(lm(true_errors^2 ~ private_data2$Z1))$coefficients
