rm(list = ls())
library(GGally)
library(fitdistrplus)
library(metRology)
library(tibble)
# library(extraDistr)
source("R/halft.R")
source("R/utils.R")

baselineSamples <- readRDS("results/samplesBaseline.rds")

fit <- vector("list", ncol(baselineSamples))
for (i in seq_len(ncol(baselineSamples))) {
  fit[[i]] <- fitdist(baselineSamples[, i], "gamma")
}
ests <- sapply(fit, `[[`, "estimate")
colnames(ests) <- colnames(baselineSamples)
saveRDS(ests, file = "results/posteriorFitsBaselineGammaEstimates.rds")

probs <- seq(0.01, 0.99, 0.01)
tbh <- tbl <- tbq <- tibble()
for (i in seq_len(ncol(ests))) {
  h <- hist(baselineSamples[, i], plot = FALSE)
  tbh2add <- tibble(
    x = h$mids,
    y = h$density,
    d = diff(h$breaks),
    g = colnames(baselineSamples)[i]
  )
  tbl2add <- tibble(
    x = seq(h$breaks[1], h$breaks[length(h$breaks)], length.out = 1e3),
    y = dgamma(x, shape = ests["shape", i], rate = ests["rate", i]),
    g = colnames(baselineSamples)[i]
  )
  tbq2add <- tibble(
    x = quantile(baselineSamples[, i], probs = probs),
    y = qgamma(probs, shape = ests["shape", i], rate = ests["rate", i]),
    g = colnames(baselineSamples)[i]
  )
  tbh <- rbind(tbh, tbh2add)
  tbl <- rbind(tbl, tbl2add)
  tbq <- rbind(tbq, tbq2add)
}

fontsize <- 20

tb0 <- tibble(
  x = c(baselineSamples),
  g = rep(colnames(baselineSamples), each = nrow(baselineSamples))
)

g0 <- ggplot() +
  geom_histogram(data = tb0, mapping = aes(x = x, y =  ..density..),
                 fill = "grey70", color = "grey60") +
  geom_line(data = tbl, mapping = aes(x = x, y = y), size = 1.2) +
  facet_wrap(~g, scales = "free", nrow = 2) +
  labs(x = NULL, y = "Density") +
  theme_bw(fontsize)

g1 <- ggplot() +
  geom_col (data = tbh, mapping = aes(x = x, y = y), width = 1) +
  geom_line(data = tbl, mapping = aes(x = x, y = y), size = 1.2) +
  facet_wrap(~g, scales = "free") +
  theme_bw(fontsize)

g2 <- ggplot() +
  geom_abline(intercept = 0, slope = 1, color = "grey50") +
  geom_point(data = tbq, mapping = aes(x = x, y = y), size = 1, shape = 21) +
  facet_wrap(~g, scales = "free", nrow = 2) +
  labs(x = "Observed Quantiles", y = "Estimated Quantiles") +
  theme_bw(fontsize)

saveFigure("posteriorToPriorFitHists.pdf",  g0)
saveFigure("posteriorToPriorFitQQplot.pdf", g2)
