# Purpose: Analysis of bladder cancer recurrent events data.
# Created: 2020-12-09
# Updated: 2020-12-21
library(dplyr)
library(MCC) # devtools::install_github(repo = "zrmacc/MCC")
library(survival)
set.seed(2013)

# ----------------------------------------------------------------------------
# Data formatting.
# ----------------------------------------------------------------------------

# Format bladder data
data <- bladder1 %>%
  dplyr::filter(treatment != "pyridoxine" & stop > start) %>%
  dplyr::select("id", "stop", "status", "treatment", "size", "number") %>%
  dplyr::rename(idx = id, time = stop) %>%
  dplyr::mutate(arm = 1 * (treatment == "thiotepa")) %>%
  dplyr::select(-"treatment")
data$status[data$status > 1] <- 2

# -----------------------------------------------------------------------------
# Composite endpoint of recurrence or death.
# -----------------------------------------------------------------------------

# Death as within the event. No agumentation.
data_comp <- data
data_comp$status[data_comp$status > 1] <- 1
fit_comp_base <- MCC::CompareAUCs(
  time = data_comp$time,
  status = data_comp$status,
  arm = data_comp$arm,
  idx = data_comp$idx,
  perm = TRUE,
  boot = TRUE,
  reps = 2000,
  tau = 60
)

# Death within the event. With augmentation.
fit_comp_aug <- MCC::CompareAUCs(
  time = data_comp$time,
  status = data_comp$status,
  arm = data_comp$arm,
  idx = data_comp$idx,
  covar = data_comp %>% dplyr::select(c("size", "number")),
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
  time = data$time,
  status = data$status,
  arm = data$arm,
  idx = data$idx,
  perm = TRUE,
  boot = TRUE,
  reps = 2000,
  tau = 60
)

# Death as competing risk. With augmentation.
fit_cr_aug <- MCC::CompareAUCs(
  time = data$time,
  status = data$status,
  arm = data$arm,
  idx = data$idx,
  covar = data %>% dplyr::select(c("size", "number")),
  perm = TRUE,
  boot = TRUE,
  reps = 2000,
  tau = 60
)

# -----------------------------------------------------------------------------
# Death as a competing risk.
# -----------------------------------------------------------------------------

save.image(file = "Results/aumcf.RData")
