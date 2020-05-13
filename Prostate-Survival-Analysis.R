library(readxl)
library(tidyverse)
library(flexsurv)

prostate_data <- read_xlsx("C:/Users/Owner/Desktop/Blood Storage.xlsx")

prostate_data <- as.data.frame(prostate_data)

prostate_data$`RBC Age Group` <- factor(prostate_data$`RBC Age Group`,
                                        levels = c(1,2,3),
                                        labels = c("Young", "Middle","Old"))
prostate_data$AA <- factor(prostate_data$AA,
                           levels = c(0,1),
                           labels = c("Not Black","Black"))
prostate_data$FamHx <- factor(prostate_data$FamHx,
                              levels = c(0,1),
                              labels = c("No Disease", "Disease"))
prostate_data$TVol <- factor(prostate_data$TVol,
                             levels = c(1,2,3),
                             labels = c("Small","Medium","Large"))
prostate_data$`T Stage` <- factor(prostate_data$`T Stage`,
                                  levels = c(1,2),
                                  labels = c("Early", "Late"))
prostate_data$AnyAdjTherapy <- factor(prostate_data$AnyAdjTherapy,
                                      levels = c(0,1),
                                      labels = c("No","Yes"))
prostate_data$AdjRadTherapy <- factor(prostate_data$AdjRadTherapy,
                                      levels = c(0,1),
                                      labels = c("No","Yes"))
prostate_data$PreopTherapy <- factor(prostate_data$PreopTherapy,
                                      levels = c(0,1),
                                      labels = c("No","Yes"))
prostate_data$Censor <- factor(prostate_data$Censor,
                               levels = c(0,1),
                               labels = c("Dead","Censored"))

prostate_data$survive <- with(prostate_data, Surv(TimeToRecurrence, Censor == "Dead"))

prostate_data %>%
  select(Recurrence, Censor, TimeToRecurrence, survive)

table(prostate_data$`RBC Age Group`, prostate_data$FamHx)

expect <- chisq.test(prostate_data$`RBC Age Group`, prostate_data$FamHx)
expect$expected

table(prostate_data$`RBC Age Group`, prostate_data$AA)

expect <- chisq.test(prostate_data$`RBC Age Group`, prostate_data$AA)
expect$expected

table(prostate_data$`RBC Age Group`, prostate_data$TVol)

expect <- chisq.test(prostate_data$`RBC Age Group`, prostate_data$TVol)
expect
expect$expected

expect <- chisq.test(prostate_data$`RBC Age Group`, prostate_data$`T Stage`)
expect
expect$expected

table(prostate_data$`RBC Age Group`, prostate_data$`AnyAdjTherapy`)

expect <- chisq.test(prostate_data$`RBC Age Group`, prostate_data$`AnyAdjTherapy`)
expect
expect$expected

table(prostate_data$`RBC Age Group`, prostate_data$`AdjRadTherapy`)

expect <- chisq.test(prostate_data$`RBC Age Group`, prostate_data$`AdjRadTherapy`)
expect
expect$expected

psych::describeBy(prostate_data$Age, group = prostate_data$`RBC Age Group`)

psych::describeBy(prostate_data$PVol, group = prostate_data$`RBC Age Group`)

model1 <- survreg(Surv(TimeToRecurrence, Censor == "Dead")
                  ~ `RBC Age Group`,
                  data = prostate_data)
summary(model1)
