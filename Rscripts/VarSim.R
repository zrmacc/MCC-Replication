# Purpose: Validate analytic variance estimator.
# Updated: 2020-12-21

# Packages.
library(optparse)
library(MCC)

# -----------------------------------------------------------------------------
# Command line arguments.
# -----------------------------------------------------------------------------

# Command line options.
opt_list <- list()

# Sample size.
opt <- make_option(c("--n"), type = "integer", help = "Patients", default = 50)
opt_list <- c(opt_list, opt)

# Truncation time.
opt <- make_option(c("--time"), type = "numeric", help = "Patients", default = 1)
opt_list <- c(opt_list, opt)

# Censoring rate.
opt <- make_option(c("--censor"), type = "numeric", help = "Censoring", default = 0.00)
opt_list <- c(opt_list, opt)

# Death rate.
opt <- make_option(c("--death"), type = "numeric", help = "Death", default = 0.00)
opt_list <- c(opt_list, opt)

# Simulation replicates.
opt <- make_option(c("--reps"), type = "integer", help = "MC replicates", default = 10)
opt_list <- c(opt_list, opt)

# Output directory.
opt <- make_option(c("--out"), type = "character", help = "Output stem", default = "Test/")
opt_list <- c(opt_list, opt)

# Option parsing.
t0 <- proc.time()
parsed_opts <- OptionParser(option_list = opt_list)
params <- parse_args(object = parsed_opts)

# Output stem.
out_suffix <- paste0(
  "N", params$n, 
  "_T", params$time,
  "_C", params$censor,
  "_D", params$death,
  ".rds"
)

# -----------------------------------------------------------------------------
# Simulation.
# -----------------------------------------------------------------------------

Loop <- function(i) {
  
  data <- GenData(
    n1 = params$n,
    n0 = params$n,
    censoring_rate = params$censor,
    death_rate = params$death,
    treatment_effect = 0,
    risk_effect = 0,
    covar_effect = 0,
    tau = 10
  )
  
  stats <- try(
    MCC::CalcStratAUC(
      data = data,
      tau = params$time,
      alpha = 0.05,
      return_areas = TRUE
    )
  )
  
  if (class(stats) != "try-error") {
    out <- c(
      stats$contrasts$observed,
      stats$contrasts$se
    )
    # Difference and ratio of AUMCFs, and the corresponding standard errors.
    names(out) <- c("diff", "ratio", "se_diff", "se_ratio")
    return(out)
  }
}

sim <- lapply(seq_len(params$reps), Loop)
sim <- do.call(rbind, sim)

# -----------------------------------------------------------------------------
# Summarize.
# -----------------------------------------------------------------------------

# Summarized simulation results.
out <- data.frame(
  "mean_diff" = mean(sim[, 1]),
  "mean_ratio" = mean(sim[, 2]),
  "empirical_var_diff" = var(sim[, 1]),
  "empirical_var_ratio" = var(sim[, 2]),
  "asymptotic_var_diff" = mean(sim[, 3]^2),
  "asymptotic_var_ratio" = mean(sim[, 4]^2)
)

# Store simulation settings.
out$n <- params$n
out$time <- params$time
out$censor <- params$censor
out$death <- params$death
out$reps <- params$reps

out_stem <- paste0(params$out)
if (!dir.exists(out_stem)) {dir.create(out_stem, recursive = TRUE)}
out_file <- paste0(out_stem, out_suffix)
saveRDS(object = out, file = out_file)

# -----------------------------------------------------------------------------
# End
# -----------------------------------------------------------------------------
t1 <- proc.time()
cat(t1-t0, "\n")
