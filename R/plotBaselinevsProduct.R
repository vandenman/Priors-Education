rm(list = ls())

library(tibble)
library(ggplot2)
TeX <- latex2exp::TeX
source("R/utils.R")

plotMeasureVersusTaskVariance <- function(df, cols, xlab = "", ylab = "Density", colorlab = "", title = NULL,
                                          legendInPlot = FALSE, peakLocation = "right") { #, textFill = textColor, ...) {
  g <- ggplot(data = df, aes(x = samples, group = what, color = what, fill = what)) +
      geom_density(alpha = .7) +
      scale_fill_manual(values = cols) +
      scale_color_manual(values = cols) +
      labs(color = colorlab,#"Measurement",
           fill  = colorlab,"Measurement",
           x     = xlab,#"Posterior Text Quality",
           y     = ylab) + #"Density") +
      theme_bw(base_size = 24)
  if ("title" %in% colnames(df))
    g <- g +
      facet_grid(cols = vars(title), labeller = label_parsed) +
      theme(strip.background = element_rect(fill = "transparent", color = "transparent"))

  if (!is.null(title))
    g <- g + ggtitle(title) + theme(plot.title = element_text(hjust = .5))
  if (legendInPlot) {
    idxPos <- if (peakLocation == "left") .2 else .8
    g <- g + theme(legend.position = c(idxPos, .95), legend.background = element_rect(fill = "transparent", colour = "transparent"))
  }
  return(g)
}

samplesBaseline     <- readRDS("results/samplesBaseline.rds")
samplesBaselineTask <- readRDS("results/samplesBaselineTaskEffects.rds")
samplesProduct      <- readRDS("results/samplesProductfeedback.rds")

averageTaskEffects <- matrix(NA, nrow(samplesBaselineTask), 4L,
                             dimnames = list(NULL, LETTERS[1:4]))
for (i in 1:4) {
  idx <- 1:4 + 8L * (i - 1L)
  # should not be the mean?
  averageTaskEffects[, i] <- rowMeans(samplesBaselineTask[, idx])
}
head(averageTaskEffects)

df <- tibble(
  what = rep(c(paste("Baseline Grade", 10:12), paste("Experiment", 1:3)), each = nrow(samplesProduct)),
  samples = c(
    # V4 = estimate of V4
    samplesBaseline[, "b_Intercept"],
    # V5 = estimate of V4 + discrepancy of V5
    samplesBaseline[, "b_Intercept"] + samplesBaseline[, "b[1]"],
    # V6 = estimate of V4 + discrepancy of V6
    samplesBaseline[, "b_Intercept"] + samplesBaseline[, "b[2]"],
    # T1 = intercept - average task effect of baseline
    samplesProduct[, "b_Intercept"] - averageTaskEffects[, "B"],
    samplesProduct[, "b_Intercept"] + samplesProduct[, "b[1]"] - averageTaskEffects[, "A"],
    samplesProduct[, "b_Intercept"] + samplesProduct[, "b[2]"] - averageTaskEffects[, "D"]
  )
)

# means of experimental study after correcting for task category
m1 <- mean(samplesProduct[, "b_Intercept"]) # mean measurement 1
ms <- c(m1, colMeans(samplesProduct[, c("b[1]", "b[2]")]) + m1)
nm <- unique(df[["what"]]); nm <- nm[startsWith(nm, "Experiment")]
tb <- data.frame(Mean = c(tapply(df[["samples"]], df[["what"]], mean)[nm], ms),
                 what = c(nm, paste("Exp", 1:3, "uncorrected")), row.names = NULL)[c(4:6, 1:3), ]
# write.csv(tb, "tables/postMeansProductCategoryCorrected.csv", row.names = FALSE, quote = FALSE)

# compare posterior distribution intercept baseline vs product
g <- plotMeasureVersusTaskVariance(df, cols, xlab = "Posterior Text Quality")
# saveFigure("comparePosteriorTextQuality.pdf", graph = g, width = 14, height = 7)


# probability of task effect ----
# color of task effect
colTask <- col2hex("gray60")

# widht & height of pdf
width  <- 6
height <- 9
xlab   <- "Posterior Task Effect"

# probability that a random task has an effect as large as the observed effect
set.seed(123)
# look at the distribution of the difference between two tasks
# ss1 <- rnorm(nrow(samplesBaseline), 0, samplesBaseline[, "sd_TI"])
# ss2 <- rnorm(nrow(samplesBaseline), 0, samplesBaseline[, "sd_TI"])
# ss <- ss1 - ss2
# look at the distribution of the difference between a random task and the intercept
ss <- rnorm(nrow(samplesBaseline), 0, samplesBaseline[, "sd_TI"])
probMoreExtremeT2 <- mean(ss >= samplesProduct[, "b[1]"] - averageTaskEffects[, "A"])
# 0.02355

# specific task doesn't matter much
# mean(ss >= samplesProduct[, "b[1]"] - matrixStats::rowMaxs(averageTaskEffects))
# 0.02408333

nms <- c("Task Effect", "T2 - T1")
df2 <- tibble(
  what = rep(nms, each = nrow(samplesBaseline)),
  samples = c(
    ss,
    samplesProduct[, "b[1]"] - averageTaskEffects[, "A"]
  )
)

title2 <- TeX(paste0(
  "$p(\\mathrm{Random\\, Task} \\geq \\mathrm{T2} - \\mathrm{T1}) = ", round(probMoreExtremeT2, 3), "$"
))

g2 <- plotMeasureVersusTaskVariance(df2, c(cols[5], colTask), xlab = xlab,
                                    title = title2, legendInPlot = TRUE, peakLocation = "left")

# probability that a random task has an effect as large as theobserved effect
probMoreExtremeT3 <- mean(ss >= samplesProduct[, "b[2]"] - averageTaskEffects[, "D"])
# 0.008133333

# specific task doesn't matter much
# mean(ss >= samplesProduct[, "b[2]"] - matrixStats::rowMaxs(averageTaskEffects))
# 0.017775

nms <- c("Task Effect", "T3 - T1")
df3 <- data.frame(
  what = rep(nms, each = nrow(samplesBaseline)),
  samples = c(
    ss,
    samplesProduct[, "b[2]"] - averageTaskEffects[, "D"]
  )
)

title3 <- TeX(paste0(
  "$p(\\mathrm{Random\\, Task} \\geq \\mathrm{T3} - \\mathrm{T1}) = ", round(probMoreExtremeT3, 3), "$"
))
g3 <- plotMeasureVersusTaskVariance(df3, c(cols[6], colTask), xlab = xlab,
                                    title = title3, legendInPlot = TRUE, peakLocation = "left")


diff23 <- (samplesProduct[, "b[2]"] - averageTaskEffects[, "D"]) - (samplesProduct[, "b[1]"] - averageTaskEffects[, "A"])
probMoreExtremeT23 <- mean(ss >= diff23)
title23 <- TeX(paste0(
  "$p(\\mathrm{Random\\, Task} \\geq \\mathrm{T3} - \\mathrm{T2}) = ", round(probMoreExtremeT23, 3), "$"
))
nms <- c("Task Effect", "T3 - T2")
df4 <- data.frame(
  what = rep(nms, each = nrow(samplesBaseline)),
  samples = c(
    ss,
    (samplesProduct[, "b[2]"] - averageTaskEffects[, "D"]) - (samplesProduct[, "b[1]"] - averageTaskEffects[, "A"])
  )
)
rgb2 <- function(x) rgb(x[1], x[2], x[3], maxColorValue = 255)
g4 <- plotMeasureVersusTaskVariance(df4, cols = c(rgb2(colorRamp(cols[5:6])(.5)), colTask),
                                    xlab = xlab, title = title23, legendInPlot = TRUE)



df234 <- rbind(df2, df3, df4)
nr <- nrow(samplesBaseline)
titles <- sapply(c(title2, title3, title23), function(x) do.call(paste, list(deparse(x), collapse = "")))
df234$title <- rep(titles, each = 2*nr)
g234 <- plotMeasureVersusTaskVariance(df234, cols = c(cols[5:6], rgb2(colorRamp(cols[5:6])(.5)), colTask), xlab = xlab,
                                      legendInPlot = TRUE) + theme(legend.position = c(.94, .95), strip.text = element_text(size = 24))


saveFigure("compareTaskEffectToT2.pdf",  g2,       width, height)
saveFigure("compareTaskEffectToT3.pdf",  g3,       width, height)
saveFigure("compareTaskEffectToT23.pdf", g4,       width, height)
saveFigure("compareTaskEffects.pdf",     g234, 3 * width, height)

# visualize the average effect of grade ----
# average of Grade 10 to Grade 11 and grade 11 to grade 12
# averageEffectGrade <- (samplesBaseline[, "b[1]"] + samplesBaseline[, "b[2]"] / 2) / 2
# Grade 10 to Grade 11
averageEffectGrade <- samplesBaseline[, "b[1]"]
df4 <- data.frame(samples = averageEffectGrade)

g4 <- ggplot(data = df4, aes(x = samples)) +
  geom_density(alpha = .7) +
  labs(color = "", fill = "", x = "Posterior Effect", y = "Density") +
  theme_bw(base_size = 24)
print(g4)

dev.new()
plot(density((samplesProduct[, "b[1]"] - averageTaskEffects[, "A"])))

effectT21inGrade <- (samplesProduct[, "b[1]"] - averageTaskEffects[, "A"]) / averageEffectGrade
effectT31inGrade <- (samplesProduct[, "b[2]"] - averageTaskEffects[, "D"]) / averageEffectGrade
dev.new()
layout(t(1:2))
plot(density(effectT21inGrade))
plot(density(effectT31inGrade))


idx <- cut(seq_len(nrow(samplesBaseline)),
    breaks = seq(0, nrow(samplesBaseline), length.out = 33), labels = FALSE)
table(idx)

averageEffectGradeAndTask <- averageEffectGrade +
  samplesBaselineTask[cbind(seq_len(nrow(samplesBaseline)), idx)]

d0 <- density(averageEffectGrade)
dev.new()
plot(density(averageEffectGradeAndTask), ylim = c(0, max(d0$y)))
lines(d0, col = 2)

effectT21inGrade <- (samplesProduct[, "b[1]"] - averageTaskEffects[, "A"]) / averageEffectGradeAndTask
effectT31inGrade <- (samplesProduct[, "b[2]"] - averageTaskEffects[, "D"]) / averageEffectGradeAndTask

xr1 <- quantile(effectT21inGrade, probs = c(.01, .99))
xr2 <- quantile(effectT31inGrade, probs = c(.01, .99))

d1 <- density(effectT21inGrade, from = xr1[1], to = xr1[2])
d2 <- density(effectT31inGrade, from = xr2[1], to = xr2[2])

dev.new()
layout(t(1:2))
plot(d1)
plot(d2)

# credible interval
cri21 <- coda::HPDinterval(coda::as.mcmc(effectT21inGrade))
cri31 <- coda::HPDinterval(coda::as.mcmc(effectT31inGrade))

computeCRIandDensity <- function(x) {
  cri <- coda::HPDinterval(coda::as.mcmc(x))
  d <- density(x, from = -1, to = 5)
  df <- data.frame(x = d$x, y = d$y)
  dfh <- data.frame(xmin = cri[1], xmax = cri[2], y = 0.95)#1.1 * max(df$y))
  return(list(df = df, dfh = dfh))
}

progressPlot <- function(df, dfh, xlab = "Improvement in years", ylab = "Density") {

  g <- ggplot(data = df, aes(x = x, y = y)) +
    geom_line() +
    geom_errorbarh(data = dfh, mapping = aes(xmin = xmin, xmax = xmax, y = y), height = .1, inherit.aes = FALSE) +
    scale_x_continuous(limits = c(-1, 5), breaks = -1:5) +
    labs(x = xlab, y = ylab) +
    theme_bw(base_size = 24)

  if ("g" %in% colnames(df))
    g <- g +
      facet_grid(cols = vars(g), labeller = label_parsed) +
      theme(strip.background = element_rect(fill = "transparent", color = "transparent"))
  return(g)
}

d21 <- computeCRIandDensity(effectT21inGrade)
d31 <- computeCRIandDensity(effectT31inGrade)

titles <- c("frac(T2, 'Grade 11' - 'Grade 10')", "frac(T3, 'Grade 11' - 'Grade 10')")
d21_31 <- d21
d21_31$df  <- rbind(d21_31$df, d31$df)
d21_31$dfh <- rbind(d21_31$dfh, d31$dfh)
d21_31$df$g <- rep(titles, each = 512)
d21_31$dfh$g <- titles

g21 <- progressPlot(d21$df, d21$dfh)
g31 <- progressPlot(d31$df, d31$dfh)
g21_31 <- progressPlot(d21_31$df, d21_31$dfh)
g21_31

tb2save <- d21_31$dfh[, 1:2]
names(tb2save) <- c("lower", "upper")
tb2save$g <- 1:2

writeTable(tb2save, "tables/credibleIntervalsImprovement.csv")
saveFigure("improvementInYears.pdf", g21_31, 2 * width, width)
