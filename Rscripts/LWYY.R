# Purpose: LWYY estimate of rate ratio. 
# Updated: 2020-12-18

# -----------------------------------------------------------------------------
# Data preparation.
# -----------------------------------------------------------------------------

library(survival)
data <- subset(
  x = bladder1,
  treatment %in% c("placebo", "thiotepa") & stop > start
)
data$arm <- 1 * (data$treatment == "thiotepa")

# Death is treated as censoring. 
data$status[data$status > 1] <- 0 

# -----------------------------------------------------------------------------
# Data preparation.
# -----------------------------------------------------------------------------

#' Data supplied to `Surv` using gap-time formatting.
#' Observations are clustered within subjects using `cluster(id)`.
#' LWYY uses the robust variance estimator.

# LWYY without adjustment.
lwyy0 <- coxph(
  Surv(time = start, time2 = stop, event = status) ~ arm + cluster(id),
  data = data, 
  robust = TRUE
)
summary(lwyy0)

# LWYY adjusted for covariates.
lwyy1 <- coxph(
  Surv(time = start, time2 = stop, event = status) ~ arm + cluster(id) + number + size,
  data = data, 
  robust = TRUE
)
summary(lwyy1)

# -----------------------------------------------------------------------------
# Save results.
# -----------------------------------------------------------------------------

save.image(file = "Results/lwyy.RData")