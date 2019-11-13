rm(list = ls())

# setup ----
library(GGally)
library(tibble)
library(coda)
library(xtable)
Tex <- latex2exp::TeX
source("R/utils.R")
source("R/commonPlotSettings.R")

posteriorScatterPlot <- function(samples, thm, ...) {

  thm <- theme_bw(18) + theme(panel.border = element_blank())

  return(ggpairs(
    data = as.data.frame(samples),
    lower = list(
      continuous = function(data, mapping, ...) {
        ggplot(data = data, mapping = mapping) +
          geom_hex() +
          scale_fill_gradient(low = "grey60", high = "grey20") +
          thm
      },
      combo = ggally_dot_no_facet
    ),
    diag = list(
      continuous = function(data, mapping, ...) ggally_densityDiag(data, mapping, ...) + thm,
      # GGally defaults
      discrete = "barDiag", na = "naDiag"
    ),
    upper = list(
      continuous = function(data, mapping, ...) {
        xdata <- rlang::eval_tidy(mapping$x, data)
        ydata <- rlang::eval_tidy(mapping$y, data)
        cc <- cor(xdata, ydata)
        # round using significant digits, e.g., "-0.00065"
        # label <- paste(signif(cc, 3), sep = "", collapse = "")
        # round using fixed number of digits e.g., "-0.001"
        label <- formatC(cc, digits = 3, format = "f")
        p <- ggally_text(label = label, mapping, xP = 0.5, yP = 0.5, xrange = 0:1, yrange = 0:1,
                         size = 12, ...) +
          theme(legend.position = "none") + theme_void()
        return(p)
      },
      # GGally defaults
      combo = "box_no_facet", discrete = "facetbar", na = "na"
    ),
    ...
  ) + theme(
      text = element_text(size = 28),
      strip.background = element_rect(fill = "transparent",colour = "transparent")
    )
  )
}

posteriorDensityPlot <- function(df, colors) {

  return(ggplot(data = df, aes(x = samples, group = what, color = what, fill = what)) +
    geom_density(alpha = .7) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    labs(color = "Measurement",
         fill  = "Measurement",
         x     = "Posterior Text Quality",
         y     = "Density") +
    theme_bw(base_size = 24) +
    theme(
      legend.direction = 'vertical',
      legend.position = 'right',
      # legend.title = element_blank(),
      legend.key = element_rect(size = 5),
      # legend.spacing.y = unit(5, "lines"),
      # legend.margin = margin(b = 3, unit = "cm"),
      legend.key.size = unit(2, "lines"),
      legend.spacing.y = unit(2.0, 'cm'),
      # legend.text = element_text(margin = margin(b = 10))
      # legend.key.size = unit(2, "cm")
    ))
}

squareStandardDeviations <- function(samples) {
  idx <- startsWith(colnames(samples), "sd_") | startsWith(colnames(samples), "sigma")
  samples[, idx] <- samples[, idx]^2
  return(samples)
}

posteriorSummaryTable <- function(samples, squareSds = TRUE) {
  if (squareSds)
    samples <- squareStandardDeviations(samples)

  samples2 <- as.mcmc(samples)
  mus <- colMeans(samples)
  sds <- apply(samples, 2, sd)
  hpd <- HPDinterval(samples2)
  df <- data.frame(Parameter = names(mus), Mean = mus, SD = sds, hpd, row.names = NULL)
  colnames(df)[4:5] <- c("Lower", "Upper")
  df$Parameter <- gsub("_", "\\_", df$Parameter, fixed = TRUE)
  return(df)
}

# baseline ----
samplesBaseline <- readRDS("results/samplesBaseline.rds")

# scattor plot of posterior samples
columnLabels <- c("Intercept", "Grade~11", "Grade~12", "sigma[w]^2~(school)", "sigma[u]^2~(student)",
                  "sigma[v]^2~(task)", "sigma[epsilon]^2")
idxDraw <- match(c("b_Intercept", "b[1]", "b[2]", "sd_SI", "sd_SI:PI", "sd_TI", "sigma"),
                 colnames(samplesBaseline))

gDescriptive <- posteriorScatterPlot(squareStandardDeviations(samplesBaseline[, idxDraw]),
                                     columnLabels = columnLabels, labeller = label_parsed)
saveFigure("baselinePosteriorDescriptivesPlot.pdf",  gDescriptive, width = 20, height = 20)

# improvement over grades
samplesBaselineTask <- readRDS("results/samplesBaselineTaskEffects.rds")
averageTaskEffects <- matrix(NA, nrow(samplesBaselineTask), 4L,
                             dimnames = list(NULL, LETTERS[1:4]))
for (i in 1:4) {
  idx <- 1:4 + 8L * (i - 1L)
  averageTaskEffects[, i] <- rowMeans(samplesBaselineTask[, idx])
}

df <- tibble(
  what = rep(paste("Grade", 10:12), each = nrow(samplesBaseline)),
  samples = c(
    # V4 = estimate of V4
    samplesBaseline[, "b_Intercept"],
    # V5 = estimate of V4 + discrepancy of V5
    samplesBaseline[, "b_Intercept"] + samplesBaseline[, "b[1]"],
    # V6 = estimate of V4 + discrepancy of V6
    samplesBaseline[, "b_Intercept"] + samplesBaseline[, "b[2]"]
  )
)

# compare posterior distribution across grades
g <- posteriorDensityPlot(df, colsBaseline)

saveFigure("baselinePosteriorTextQualityOverGrades.pdf", graph = g, width = 14, height = 7)

# table with posterior summary
tb <- posteriorSummaryTable(samplesBaseline)[c(8, 1, 2, 6, 5, 7, 4), ]
tb$Parameter <- c("Intercept", "Grade 11", "Grade 12", "$\\sigma^2_w \\, \\mathrm{(school)}$",
                  "$\\sigma^2_u \\, \\mathrm{(student)}$", "$\\sigma^2_v \\, \\mathrm{(task)}$", "$\\sigma^2_\\epsilon$")
print(xtable(tb, digits = 3))
write.csv(tb, "tables/postSummaryBaseline.csv", row.names = FALSE, quote = FALSE)

# product ----
samplesProduct  <- readRDS("results/samplesProductfeedback.rds")

# scattor plot of posterior samples
idx <- startsWith(colnames(samplesProduct), "sd_")
colnames(samplesProduct)[idx] <- c("sd_SI", "sd_SI:PI")
columnLabels <- c("Intercept", "T2", "T3", "sigma[w]^2~(school)", "sigma[u]^2~(student)", "sigma[epsilon]^2")
idxDraw <- match(c("b_Intercept", "b[1]", "b[2]", "sd_SI", "sd_SI:PI", "sigma"), colnames(samplesProduct))

gDescriptive <- posteriorScatterPlot(squareStandardDeviations(samplesProduct[, idxDraw]),
                                     columnLabels = columnLabels, labeller = label_parsed)
saveFigure("productPosteriorDescriptivesPlot.pdf",  gDescriptive, width = 20, height = 20)

# improvement over measurement occasions
df <- tibble(
  what = factor(rep(1:3, each = nrow(samplesProduct))),
  samples = c(
    # estimate of first measurement
    samplesProduct[, "b_Intercept"],
    # estimate of first measurement + discrepancy of second measurement
    samplesProduct[, "b_Intercept"] + samplesProduct[, "b[1]"],
    # estimate of first measurement + discrepancy of third measurement
    samplesProduct[, "b_Intercept"] + samplesProduct[, "b[2]"]
  )
)

# compare posterior distribution across grades
df$what <- paste0("T", df[["what"]])
g <- posteriorDensityPlot(df, colsExperim)

saveFigure("productPosteriorTextQuality.pdf", graph = g, width = 14, height = 7)

# table with posterior summary
tb <- posteriorSummaryTable(samplesProduct)[c(8, 1, 2, 6, 7, 5), ]
tb$Parameter <- c("Intercept", "Measurement 2", "Measurement 3", "$\\sigma^2_w \\, \\mathrm{(school)}$",
                  "$\\sigma^2_u \\, \\mathrm{(student)}$", "$\\sigma^2_\\epsilon$")
print(xtable(tb, digits = 3))
write.csv(tb, "tables/postSummaryProduct.csv", row.names = FALSE, quote = FALSE)
