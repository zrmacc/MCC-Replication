# Purpose: Prepare data for bladder cancer analysis.

library(dplyr)
library(survival)

# Format bladder data.
data <- bladder1 %>%
  dplyr::filter(treatment != "pyridoxine" & stop > start) %>%
  dplyr::select("id", "start", "stop", "status", "treatment", "size", "number") %>%
  dplyr::mutate(arm = 1 * (treatment == "thiotepa")) %>%
  dplyr::select(-"treatment")
data$status[data$status > 1] <- 2

# Alias.
data$idx <- data$id
data$time <- data$stop

saveRDS(object = data, file = "Data/bladder_data.rds")
