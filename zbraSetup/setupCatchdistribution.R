dl <- zbra[[1]]$dl
minlen <- zbra[[1]]$minlength
maxlen <- zbra[[1]]$maxlength
minage <- zbra[[1]]$minage
maxage <- zbra[[1]]$maxage
allareas <- zbra[[1]]$livesonareas
length.dl <- mfdb_interval('len', seq(minlen, maxlen, dl))

# script to create catchdistribution components from gadget output
library(mfdbatlantis)
source('gadgetTest/functions/gadget_add_lengthgroups.R')
source('gadgetTest/functions/gadget_stripAgeLength.R')

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
    atlantis_tracer_survey_select(length_group, survey_suitability, survey_sigma) 

al.survey <- 
    gadget_stripAgeLength(surveys, 0.7, 0.05) %>%
    rename(number = count)


## pulling age and length distributions from the surveys data

# spr survey length distribution
ldist.spr <- 
    al.survey %>%
    filter(year %in% defaults$data.year,
           month == spr.survey.month,
           !is.na(length)) %>%
    mutate(step = timestep[month],
           age = 'allages',
           area = 'allareas') %>%
    select(year, step, area, age, length, number) %>%
    mutate(length = length.dl[findInterval(length, length.dl)] %>%
                    attr(., 'names') %>%
                    as.factor()) %>%
    group_by(year, step, area, age, length) %>%
    summarize(number = sum(number)) %>%
    filter(number >= 1) %>%
    structure(area = mfdb_group(allareas = allareas),
              age = mfdb_group(allages = minage:maxage),
              length = lapply(as.list(length.dl), FUN=function(x) {
                              structure(seq(x, x + (dl-1)),
                              min = x,
                              max = x + dl)}))


# spr survey age-length distribution
aldist.spr <- 
    al.survey %>%
    filter(year %in% defaults$data.year,
           month == spr.survey.month,
           !is.na(age)) %>%
    mutate(step = timestep[month],
           area = 'allareas',
           age = paste0('age', age)) %>%
    select(year, step, area, age, length, number) %>%
    mutate(length = length.dl[findInterval(length, length.dl)] %>%
               attr(., 'names') %>%
               as.factor()) %>%
    group_by(year, step, area, age, length) %>%
    summarize(number = sum(number)) %>%
    filter(number >= 1) %>%
    structure(area = mfdb_group(allareas = allareas),
              age = lapply(mfdb_interval('age', minage:maxage), function(x) {
                  as.numeric(as.character(gsub('age', '', x)))
              }),
              length = lapply(as.list(length.dl), FUN=function(x) {
                  structure(seq(x, x + (dl-1)),
                            min = x,
                            max = x + dl)}))


## autumn length distribution
ldist.aut <- 
    al.survey %>%
    filter(year %in% defaults$data.year,
           month == aut.survey.month,
           !is.na(length)) %>%
    mutate(step = timestep[month],
           age = 'allages',
           area = 'allareas') %>%
    select(year, step, area, age, length, number) %>%
    mutate(length = length.dl[findInterval(length, length.dl)] %>%
               attr(., 'names') %>%
               as.factor()) %>%
    group_by(year, step, area, age, length) %>%
    summarize(number = sum(number)) %>%
    filter(number >= 1) %>%
    structure(area = mfdb_group(allareas = allareas),
              age = mfdb_group(allages = minage:maxage),
              length = lapply(as.list(length.dl), FUN=function(x) {
                  structure(seq(x, x + (dl-1)),
                            min = x,
                            max = x + dl)}))

# aut survey age-length distribution
aldist.aut <- 
    al.survey %>%
    filter(year %in% defaults$data.year,
           month == aut.survey.month,
           !is.na(age)) %>%
    mutate(step = timestep[month],
           area = 'allareas',
           age = paste0('age', age)) %>%
    select(year, step, area, age, length, number) %>%
    mutate(length = length.dl[findInterval(length, length.dl)] %>%
               attr(., 'names') %>%
               as.factor()) %>%
    group_by(year, step, area, age, length) %>%
    summarize(number = sum(number)) %>%
    filter(number >= 1) %>%
    structure(area = mfdb_group(allareas = allareas),
              age = lapply(mfdb_interval('age', minage:maxage), function(x) {
                  as.numeric(as.character(gsub('age', '', x)))
              }),
              length = lapply(as.list(length.dl), FUN=function(x) {
                  structure(seq(x, x + (dl-1)),
                            min = x,
                            max = x + dl)}))

## commercial catch sampling
sea.sample.prop <- 0.001

ldist.catch <-
    prey %>%
    filter(year %in% defaults$data.years) %>% 
    mutate(number = number.consumed * sea.sample.prop, 
           step = timestep[month],
           age = 'allages',
           area = 'allareas') %>%
    select(year, step, area, age, length, number) %>%
    filter(number >= 1) %>%
    group_by(year, step, area, age, length) %>%
    summarize(number = round(sum(number))) %>%
    filter(number >= 1) %>%
    structure(area = mfdb_group(allareas = allareas),
              age = mfdb_group(allages = minage:maxage),
              length = lapply(as.list(length.dl), FUN=function(x) {
                  structure(seq(x, x + (dl-1)),
                            min = x,
                            max = x + dl)}))

aldist.catch <-
    prey %>%
    filter(year %in% defaults$data.years) %>% 
    mutate(number = number.consumed * sea.sample.prop, 
           step = timestep[month],
           area = 'allareas',
           age = paste0('age', age)) %>%
    select(year, step, area, age, length, number) %>%
    filter(number >= 1) %>%
    group_by(year, step, area, age, length) %>%
    summarize(number = round(sum(number))) %>%
    filter(number >= 1) %>%
    structure(area = mfdb_group(allareas = allareas),
              age = lapply(mfdb_interval('age', minage:maxage), function(x) {
                  as.numeric(as.character(gsub('age', '', x)))
              }),
              length = lapply(as.list(length.dl), FUN=function(x) {
                  structure(seq(x, x + (dl-1)),
                            min = x,
                            max = x + dl)}))
