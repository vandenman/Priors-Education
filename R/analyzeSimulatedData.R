rm(list = ls())
library(brms)

# model fitting ----

# for the settings used in the actual analyses, see  R/mcmcSettings.R
# the settings below are a bit more practical and will ensure the model runs in a reasonable time
# ~30 minutes on my machine
iter   <- 1e4L
warmup <- 5e3L
chains <- 10L
control <- list(adapt_delta = 0.9, max_treedepth = 15)
# run the chains in parallel
cores <- min(parallel::detectCores() - 1L, chains)

dataDir    <- "dataSimulated"
resultsDir <- "resultsSimulated"

# load simulated data
datBaseline     <- readRDS(file.path(dataDir, "baselineData.rds"))
datExperimental <- readRDS(file.path(dataDir, "experimentalData.rds"))

fileFitSimulatedBaseline     <- file.path(resultsDir, "fitBaselineData.rds")
fileFitSimulatedExperimental <- file.path(resultsDir, "fitExperimentalData.rds")

# fit both models
if (file.exists(fileFitSimulatedBaseline)) {
  fitBaseline <- readRDS(fileFitSimulatedBaseline)
} else {
  fitBaseline <- brm(
    formula = Score_Mean ~ 1 + Grade + (1 | Task_index) + (1 || School_index / Participant_index),
    data = datBaseline,
    iter = iter, warmup = warmup, chains = chains, cores = cores,
    control = control
  )
  saveRDS(fitBaseline, file = fileFitSimulatedBaseline)
}

if (file.exists(fileFitSimulatedExperimental)) {
  fitExperimental <- readRDS(fileFitSimulatedExperimental)
} else {
  fitExperimental <- brm(
    formula = Score_Mean ~ 1 + Task + (1 || School_index / Participant_index),
    data = datExperimental,
    iter = iter, warmup = warmup, chains = chains, cores = cores, control = control,
    prior = set_prior("cauchy(0, 1)", class = "b")
  )
  saveRDS(fitExperimental, file = fileFitSimulatedExperimental)
}

# square all standard deviations and use sensible names
nms <- names(fitBaseline$fit)
idx <- !(startsWith(nms, prefix = "z_") | startsWith(nms, prefix = "r_") | startsWith(nms, prefix = "lp__") | startsWith(nms, prefix = "lprior"))

samplesBaseline <- as.matrix(fitBaseline$fit, pars = nms[idx])
newNames <- c("Intercept", "Grade 11", "Grade 12", "varSchool", "varStudent", "varTask", "varResidual")
colnames(samplesBaseline) <- newNames

idx2Square <- startsWith(newNames, "var")
samplesBaseline[, idx2Square]  <- samplesBaseline[, idx2Square]^2

nms <- names(fitExperimental$fit)
idx <- !(startsWith(nms, prefix = "z_") | startsWith(nms, prefix = "r_") | startsWith(nms, prefix = "lp__") | startsWith(nms, prefix = "lprior"))

samplesExperimental <- as.matrix(fitExperimental$fit, pars = nms[idx])

newNames <- c("Intercept", "T2", "T3", "varSchool", "varStudent", "varResidual")
colnames(samplesExperimental) <- newNames
idx2Square <- startsWith(newNames, "var")
samplesExperimental[, idx2Square]  <- samplesExperimental[, idx2Square]^2

if (!dir.exists(resultsDir)) dir.create(resultsDir)
saveRDS(samplesBaseline,     file = file.path(resultsDir, "samplesBaseline.rds"))
saveRDS(samplesExperimental, file = file.path(resultsDir, "samplesExperimental.rds"))

tasknames <- sort(unique(datBaseline$Task_Group))
idx2 <- startsWith(names(fitBaseline$fit), prefix = "r_Task_index")
names(fitBaseline$fit)[idx2]

samplesTaskEffects <- as.matrix(fitBaseline$fit, pars = names(fitBaseline$fit)[idx2])
colnames(samplesTaskEffects) <- rep(tasknames, each = 8)
saveRDS(samplesTaskEffects, file.path(resultsDir, "samplesBaselineTaskEffectsRaw.rds"))

averageTaskEffects <- matrix(NA, nrow(samplesTaskEffects), 4L, dimnames = list(NULL, LETTERS[1:4]))
for (i in 1:4) {
  idx <- 1:4 + 8L * (i - 1L)
  averageTaskEffects[, i] <- rowMeans(samplesTaskEffects[, idx])
}
saveRDS(averageTaskEffects, file.path(resultsDir, "samplesBaselineAverageTaskEffects.rds"))
