# takes the supplied datasets, computes all required statistics and save the cleaned data as .rds files.

rm(list = ls())

library(tidyr)
library(dplyr)
library(readr)

# clean baseline dataset
dBaseline <- readr::read_csv2("data/Baselinestudie_vwo456.csv")

RuweData <- dBaseline
RuweData$Gender <- factor(RuweData$Gender)
RuweData$Grade  <- factor(RuweData$Grade,levels = c("4","5","6","BA1","BA2","BA3"))
TQ_VO <- RuweData[!(RuweData$Grade %in% c("BA1", "BA2", "BA3")), ]
TQ_VO$GradeL <- as.numeric(TQ_VO$Grade) - 1L
TQ_VO$Nbronnen <- NA
TQ_VO$Nbronnen[TQ_VO$Topic == "A"] <- 3
TQ_VO$Nbronnen[TQ_VO$Topic == "B"] <- 5
TQ_VO$Nbronnen[TQ_VO$Topic == "C"] <- 4
TQ_VO$Nbronnen[TQ_VO$Topic == "D"] <- 4
TQ_VO$Nbronnen <- as.factor(TQ_VO$Nbronnen)
Dat <- na.omit(TQ_VO[,c("ID_Participant","Task_Code","Schoolnummer","Score_Mean","Grade","GradeL","Gender","Genre","Task_Order","Nbronnen","Complementariteit_bronnen","Hoeveelheid_overbodige_info")])
Dat$Grade <- droplevels(Dat$Grade)
Dat$Participant_index <- as.integer(as.factor(Dat$ID_Participant))
Dat$Task_index <- as.integer(as.factor(Dat$Task_Code))
Dat$School_index <- as.integer(as.factor(Dat$Schoolnummer))

Dat$Grade <- recode(Dat$Grade,
  "4" = "Grade 10",
  "5" = "Grade 11",
  "6" = "Grade 12"
)

saveRDS(Dat, file = "data/cleanedBaseline.rds")

# clean productfeedback dataset
dProductFeedback <- readr::read_csv("data/Dataset productfeedback 2019.csv")

dProductFeedback2 <-
  dProductFeedback %>%
  select("ID_Participant", "School", "Klas", "Conditie",
         "herschaalde score taak 1", "herschaalde score taak 2", "herschaalde score taak 3") %>%
  na_if("-99.00") %>%
  na_if("-99") %>%
  gather(Taak, Score_Mean, -ID_Participant, -School, -Conditie, -Klas, -Conditie) %>%
  rename(
    Participant_index = ID_Participant,
    School_index      = School
  )

saveRDS(dProductFeedback2, file = "data/cleanedProductfeedback.rds")
