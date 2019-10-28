# Analyzes the baseline dataset and stores the posterior samples.
rm(list = ls())

library(brms)
library(bayesplot)
library(rstan)
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-O3 -march=native -Ofast -frename-registers -funroll-loops')

dat <- readRDS("data/cleanedProductfeedback.rds")

# (tapply(dat$Score_Mean, list(dat$Conditie, dat$Taak), mean, na.rm = TRUE))


# modified version of brms Stan code
model <- stan_model("stanmodels/secondModel_brm.stan")

# create data object
f <- Score_Mean ~ 1 + Taak * Conditie + Taak * Conditie + (1 || School_index / Participant_index)
# f <- Score_Mean ~ 1 + Taak + Conditie + (1 || School_index / Participant_index)
dataList <- make_standata(
  formula      = f,
  data         = dat,
  prior        = NULL,
  cov_ranef    = NULL,
  sample_prior = "no",
  knots        = NULL,
  stanvars     = NULL
)

# starting model
# make_stancode(f, data = dat)

# add prior to dataList
estimates <- readRDS("results/posteriorFitsBaselineGammaEstimates.rds")
estimates <- estimates[, c("sd_1[1]", "sd_2[1]", "b[1]", "sigma", "temp_Intercept")]
dataList$shapes <- estimates["shape", ]
dataList$rates  <- estimates["rate", ]

# setup for mcmc samples, in total (iter - warmup) * chains samples are obtained.
iter   <- 5e4L
warmup <- 1e4L
chains <- 3L

# initial values
# freq <- lm(Score_Mean ~ 1 + Taak + Conditie + Taak * Conditie, data = dat)
# inits <- replicate(chains, list(list(b = rnorm(5, coef(freq)[-1L], .1))))
# run the chains in parallel
# options("mc.cores" = 1)
options("mc.cores" = min(parallel::detectCores() - 1L, chains))

# sample from the posterior
res <- sampling(
  object = model,
  data   = dataList,
  iter   = iter,
  warmup = warmup,
  chains = chains#,
  # init   = inits
)

# write all results to disk
saveRDS(res, "results/analysisProductfeedback.rds")

# write reduced samples to disk
nms <- names(res)
idx <- !(startsWith(nms, prefix = "z_") | startsWith(nms, prefix = "r_") | startsWith(nms, prefix = "lp__"))
samples <- as.matrix(res, pars = nms[idx])

# write all results to disk
saveRDS(samples, "results/samplesProductfeedback.rds")


# diagnostics
check_hmc_diagnostics(res)
# Divergences:
#   0 of 120000 iterations ended with a divergence.
#
# Tree depth:
#   4 of 120000 iterations saturated the maximum tree depth of 10 (0.00333333333333333%).
# Try increasing 'max_treedepth' to avoid saturation.
#
# Energy:
#   E-BFMI indicated no pathological behavior.

# visual diagnostics
show <- nms[idx] # remove [idx] to see results for all parameters
rhats <- rhat(res, pars = show)
ratios_cp <- neff_ratio(res, pars = show)
mcmc_rhat(rhats)
mcmc_neff(ratios_cp, size = 2)

# summary of posterior
summ <- summary(res)
summ$summary[show, ]

