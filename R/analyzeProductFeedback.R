# Analyzes the baseline dataset and stores the posterior samples.
rm(list = ls())

library(rstan)
library(brms)
rstan_options(auto_write = TRUE)

dat <- readRDS("data/cleanedProductfeedback.rds")

seed <- 42

# setup for mcmc samples, in total (iter - warmup) * chains samples are obtained.
# iter   <- 6e4L
# warmup <- 1e4L
# chains <- 6L
iter   <- 6e3L
warmup <- 1e3L
chains <- 3L

# arguemnts for Stan's sampling algorithm
control <- list(
  adapt_delta = 0.81
)

# run the chains in parallel
cores <- min(parallel::detectCores() - 1L, chains)

f <- Score_Mean ~ 1 + Taak * Conditie + Taak * Conditie + (1 || School_index / Participant_index)
res <- brm(formula = f, data = dat, iter = iter, warmup = warmup, chains = chains, cores = cores,
           silent = FALSE, seed = seed, control = control)

# write results to disk
saveRDS(res, "results/analysisProductfeedback.rds")
# res <- readRDS("results/analysisProductfeedback.rds")

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
saveRDS(samplesArray, "results/productSamplesArray.rds")
saveRDS(samplesMatrix, "results/productSamples.rds")

