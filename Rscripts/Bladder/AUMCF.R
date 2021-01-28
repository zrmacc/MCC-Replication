# Purpose: Analysis of bladder cancer recurrent events data.
# Created: 2020-12-09
# Updated: 2020-12-21
library(dplyr)
library(MCC)
library(survival)
data <- readRDS(file = "Data/bladder_data.rds")

# Composite endpoint data.
data_comp <- data
data_comp$status[data_comp$status > 1] <- 1

# -----------------------------------------------------------------------------
# Composite endpoint of recurrence or death.
# -----------------------------------------------------------------------------

# Death as within the event. No agumentation.
fit_comp_base <- MCC::CompareAUCs(
  idx = data_comp$idx,
  time = data_comp$time,
  status = data_comp$status,
  arm = data_comp$arm,
  perm = TRUE,
  boot = TRUE,
  reps = 2000,
  tau = 60
)

# Death within the event. With augmentation.
fit_comp_aug <- MCC::CompareAUCs(
  idx = data_comp$idx,
  time = data_comp$time,
  status = data_comp$status,
  arm = data_comp$arm,
  covars = data_comp %>% dplyr::select(c("size", "number")),
  perm = TRUE,
  boot = TRUE,
  reps = 2000,
  tau = 60
)

# -----------------------------------------------------------------------------
# Death as a competing risk.
# -----------------------------------------------------------------------------

# Death as competing risk. No augmentation.
fit_cr_base <- MCC::CompareAUCs(
  idx = data$idx,
  time = data$time,
  status = data$status,
  arm = data$arm,
  perm = TRUE,
  boot = TRUE,
  reps = 2000,
  tau = 60
)

# Death as competing risk. With augmentation.
fit_cr_aug <- MCC::CompareAUCs(
  idx = data$idx,
  time = data$time,
  status = data$status,
  arm = data$arm,
  covars = data %>% dplyr::select(c("size", "number")),
  perm = TRUE,
  boot = TRUE,
  reps = 2000,
  tau = 60
)

# -----------------------------------------------------------------------------
# Death as a competing risk.
# -----------------------------------------------------------------------------

save.image(file = "Results/aumcf.RData")
