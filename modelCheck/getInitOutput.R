## this is code to check output from zbraInit model
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mfdb)
library(mfdbatlantis)
setwd('~/gadget/models/')

## source output from initial model
gs.data <- gadget_directory('gadgetTest/zbraInit')
source('gadgetTest/zbraSetup/getSimData.R')

# script to create catchdistribution components from gadget output
source('gadgetTest/functions/gadget_add_lengthgroups.R')
source('gadgetTest/functions/gadget_stripAgeLength.R')
source('functions/vbParams.R')

spr.survey.month <- 4
aut.survey.month <- 10

# first create a survey
length_group <- seq(0.5, 200.5, by=1)
sigma_per_cohort <- 
    std %>%
    group_by(age) %>%
    summarize(mean.sd = mean(length.sd))
survey_suitability <- rep(0.001, length(length_group))
survey_sigma <- 0

surveys <- 
    std %>%
    filter(month %in% c(spr.survey.month, aut.survey.month)) %>%
    mutate(cohort = age) %>%
    rename(count = number, 
           length = mean.length) %>%
    select(year, month, area, age, cohort, count, length) %>%
    gadget_add_lengthgroups(length_group, sqrt(sigma_per_cohort$mean.sd)) %>%
    atlantis_tracer_survey_select(length_group, survey_suitability, survey_sigma) %>%
    filter(count >= 1)

ldist.plot <- 
    ggplot(data=filter(surveys, month==4), 
           aes(x=length, y=count, color=factor(age))) + 
    geom_line() + facet_wrap(~year) + 
    geom_vline(xintercept=vb(167.76, 0.083, -1.62, 0:20))