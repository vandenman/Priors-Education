rm(list = ls())

# clean all data sets
source("R/cleanDatasets.R")

# descriptives of baseline data set
source("R/descriptivesBaseline.R")

# descriptives of baseline data set
source("R/descriptivesProduct.R")

# analyze baseline data set
source("R/analyzeBaseline.R")

# analyze product dataset
source("R/analyzeProductFeedback.R")

# figures with mcmc diagnostics
source("R/MCMCconvergence.R")

# make scatter plot of posterior samples
source("R/descriptivesPosterior.R")

# compare baseline and experimental data set
source("R/plotBaselinevsProduct.R")
