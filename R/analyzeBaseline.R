# Analyzes the baseline dataset and stores the posterior samples.
rm(list = ls())

library(brms)
dat <- readRDS("data/cleanedBaseline.rds")

# setup for mcmc samples, in total (iter - warmup) * chains samples are obtained.
iter   <- 6e4L
warmup <- 1e4L
chains <- 6L

# run the chains in parallel
cores <- min(parallel::detectCores() - 1L, chains)

f <- Score_Mean ~ 1 + Grade + (1 | Task_index) + (1 || School_index / Participant_index) # TODO: single or double | for school index?
brmres <- brm(formula = f, data = dat, iter = iter, warmup = warmup, chains = chains, cores = cores)
res <- brmres$fit

# write results to disk
saveRDS(res, "results/analysisBaseline.rds")
# res <- readRDS("results/analysisBaseline.rds")

# write reduced samples to disk
nms <- names(res)
idx <- !(startsWith(nms, prefix = "z_") | startsWith(nms, prefix = "r_") | startsWith(nms, prefix = "lp__"))

renamer <- function(nms) {
  idx <- match(nms, c("sd_1[1]", "sd_2[1]", "sd_3[1]"), nomatch = 0L)
  nms[idx > 0L] <- c("sd_SI", "sd_SI:PI", "sd_TI")
  return(nms)
}

samplesMatrix <- as.matrix(res, pars = nms[idx])
colnames(samplesMatrix) <- renamer(colnames(samplesMatrix))

samplesArray  <- as.array(res, pars = nms[idx])
dimnames(samplesArray)[[3L]] <- renamer(dimnames(samplesArray)[[3L]])

# write all results to disk
saveRDS(samplesMatrix, "results/samplesBaseline.rds")
saveRDS(samplesArray, "results/samplesArrayBaseline.rds")

# sanity check -- do the posterior means correspond to frequentist point estimates?
# samplesBaseline <- readRDS("results/samplesBaseline.rds")
# idx <- startsWith(colnames(samplesBaseline), "sd_") | startsWith(colnames(samplesBaseline), "sigma")
# samplesBaseline[, idx] <- samplesBaseline[, idx]^2
# colMeans(samplesBaseline)
#
# resFreq <- lme4::lmer(Score_Mean ~ 1 + Grade + (1 | Task_index) + (1 | School_index / Participant_index), data = dat)
# summary(resFreq)

# save task effects separately
tasknames <- sort(unique(dat$Task_Code))
idx2 <- startsWith(nms, prefix = "r_3")
nms[idx2]

samplesTaskEffects <- as.matrix(res, pars = nms[idx2])
colnames(samplesTaskEffects) <- tasknames
saveRDS(samplesTaskEffects, "results/samplesBaselineTaskEffects.rds")

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
summ <- summary(res, pars = show)
summ$summary[show, ]


# old ----
#
# # modified version of brms Stan code
# model <- stan_model("stanmodels/baselineModel_grade.stan")
#
# # create data object
# f <- Score_Mean ~ 1 + Grade + (1 | Task_index) + (1 || School_index / Participant_index)
# dataList <- make_standata(
#   formula      = f,
#   data         = dat,
#   prior        = NULL,
#   cov_ranef    = NULL,
#   sample_prior = "no",
#   knots        = NULL,
#   stanvars     = NULL
# )
#
# # "stanmodels/baselineModel_grade.stan" is derived from
# # make_stancode(f, data = dat)
#
#
# # sample from the posterior
# res <- sampling(
#   object = model,
#   data   = dataList,
#   iter   = iter,
#   warmup = warmup,
#   chains = chains
# )