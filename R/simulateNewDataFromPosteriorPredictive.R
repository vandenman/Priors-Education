rm(list = ls())
library(brms)
library(dplyr)

fileBrmsFitBaseline     <- file.path("results", "brmsFit_baseline.rds")
fileBrmsFitExperimental <- file.path("results", "brmsFit_experimental.rds")

datBaseline     <- readRDS("data/cleanedBaseline.rds")
datExperimental <- readRDS("data/cleanedExperimental.rds")

if (!file.exists(fileBrmsFitBaseline))
  stop("The brms object of the baseline dataset is missing!")

if (!file.exists(fileBrmsFitExperimental))
  stop("The brms object of the experimental dataset is missing!")

brmresBaseline     <- readRDS(fileBrmsFitBaseline)
brmresExperimental <- readRDS(fileBrmsFitExperimental)

ndraws <- 1L
newDataBaseline     <- posterior_epred(brmresBaseline,     ndraws = ndraws)
newDataExperimental <- posterior_epred(brmresExperimental, ndraws = ndraws)
str(newDataBaseline)
str(newDataExperimental)

plot(datExperimental$Score_Mean, newDataExperimental)
plot(datBaseline$Score_Mean, newDataBaseline)

simulatedBaseline <- datBaseline |>
  mutate(
    Score_Mean = c(newDataBaseline),
    Task_Group = substr(Task_Code, 1, 1)
  ) |>
  select(c(Score_Mean, Grade, Task_index, School_index, Participant_index, Task_Group))
all(simulatedBaseline$Score_Mean == newDataBaseline)

simulatedExperimental <- datExperimental |>
  select(c(Score_Mean, Taak, School_index, Participant_index)) |>
  mutate(
    Score_Mean   = c(newDataExperimental),
    School_index = as.integer(factor(School_index)),
    Task         = factor(Taak, labels = 1:3)
  ) |>
  select(-Taak)
all(simulatedExperimental$Score_Mean == newDataExperimental)

saveRDS(simulatedBaseline,     file.path("dataSimulated", "baselineData.rds"))
saveRDS(simulatedExperimental, file.path("dataSimulated", "experimentalData.rds"))
