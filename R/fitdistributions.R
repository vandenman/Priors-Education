# fit several distributions to the marginal posterior of the variance components.

rm(list = ls())
library(fitdistrplus)
source("R/halft.R")

results <- readRDS(file = "results/analysisBaseline.rds") # may take a while
allsamples <- as.matrix(results)

cnms <- colnames(allsamples)
idx <- grepl("sd_", cnms, fixed = TRUE)

# column names of interest
print(cnms[idx])

postsamples <- allsamples[, idx]
colnames(postsamples)[1:3] <- c("sd_SI", "sd_SI:PI", "sd_TI")

saveRDS(postsamples, "results/samplesSubset.rds")

rm(list = ls())
postsamples <- readRDS("results/samplesSubset.rds")

x <- postsamples[, 1]
plotdist(x, histo = TRUE, demp = TRUE)
descdist(x, boot = 1000)

# fit gamma, scaled half-t distribution, and log-normal distribution to samples
fg <- fitdist(x, "gamma")
fsht <- fitdist(x, "halft.scaled",
                start = list(df = 1, mean = mean(x), sd = sd(x), ncp = 0),
                control = list(trace = 1, REPORT = 1))
fln <- fitdist(x, "lnorm")

fitted <- list(fg, fsht, fln)

# figures of fit
layout(matrix(1:4, 2, 2))
denscomp(fitted)
ppcomp(fitted)
cdfcomp(fitted)
qqcomp(fitted)

# goodness of fit statistics
gof <- gofstat(fitted)
print(gof)

# lower is better for aic and bic (check if true for all of them!)
# all statistics point out that the scaled half-t distribution performs best

gof$cvmtest
gof$adtest
gof$kstest

