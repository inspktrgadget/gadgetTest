## IGFS survey indices
## NOTE: you need to fix how length attributes are added onto
## the data if you are going to use this in any serious manner
source(sprintf('%s/setupIndexGrps.R', setup.d))

# gather spring surveys
spr.si.short <- 
    surveys %>%
    filter(year %in% defaults$data.year,
           month == spr.survey.month,
           length < max(ind.lengths$short)) %>%
    mutate(step = timestep[month],
           area = 'allareas',
           length = ind.lengths$short[findInterval(length, ind.lengths$short)] %>%
               attr(., 'names') %>%
               as.factor()) %>%
    rename(number = count) %>%
    select(year, step, area, length, number) %>%
    group_by(year, step, area, length) %>%
    summarize(number = sum(number)) %>%
    structure(area = mfdb_group(allareas = allareas),
              length = ss.lengths)
    
spr.si.mid <- 
    surveys %>%
    filter(year %in% defaults$data.year,
           month == spr.survey.month,
           length < max(ind.lengths$mid),
           length >= min(ind.lengths$mid)) %>%
    mutate(step = timestep[month],
           area = 'allareas',
           length = ind.lengths$mid[findInterval(length, ind.lengths$mid)] %>%
               attr(., 'names') %>%
               as.factor()) %>%
    rename(number = count) %>%
    select(year, step, area, length, number) %>%
    group_by(year, step, area, length) %>%
    summarize(number = sum(number)) %>%
    structure(area = mfdb_group(allareas = allareas),
              length = sm.lengths)

spr.si.long <- 
    surveys %>%
    filter(year %in% defaults$data.year,
           month == spr.survey.month,
           length < max(ind.lengths$long),
           length >= min(ind.lengths$long)) %>%
    mutate(step = timestep[month],
           area = 'allareas',
           length = ind.lengths$long[findInterval(length, ind.lengths$long)] %>%
               attr(., 'names') %>%
               as.factor()) %>%
    rename(number = count) %>%
    select(year, step, area, length, number) %>%
    group_by(year, step, area, length) %>%
    summarize(number = sum(number)) %>%
    structure(area = mfdb_group(allareas = allareas),
              length = sl.lengths)

# gather autumn surveys
aut.si.short <- 
    surveys %>%
    filter(year %in% defaults$data.year,
           month == aut.survey.month,
           length < max(ind.lengths$short)) %>%
    mutate(step = timestep[month],
           area = 'allareas',
           length = ind.lengths$short[findInterval(length, ind.lengths$short)] %>%
               attr(., 'names') %>%
               as.factor()) %>%
    rename(number = count) %>%
    select(year, step, area, length, number) %>%
    group_by(year, step, area, length) %>%
    summarize(number = sum(number)) %>%
    structure(area = mfdb_group(allareas = allareas),
              length = as.lengths)

aut.si.mid <- 
    surveys %>%
    filter(year %in% defaults$data.year,
           month == aut.survey.month,
           length < max(ind.lengths$mid),
           length >= min(ind.lengths$mid)) %>%
    mutate(step = timestep[month],
           area = 'allareas',
           length = ind.lengths$mid[findInterval(length, ind.lengths$mid)] %>%
               attr(., 'names') %>%
               as.factor()) %>%
    rename(number = count) %>%
    select(year, step, area, length, number) %>%
    group_by(year, step, area, length) %>%
    summarize(number = sum(number)) %>%
    structure(area = mfdb_group(allareas = allareas),
              length = am.lengths)

aut.si.long <- 
    surveys %>%
    filter(year %in% defaults$data.year,
           month == aut.survey.month,
           length < max(ind.lengths$long),
           length >= min(ind.lengths$long)) %>%
    mutate(step = timestep[month],
           area = 'allareas',
           length = ind.lengths$long[findInterval(length, ind.lengths$long)] %>%
               attr(., 'names') %>%
               as.factor()) %>%
    rename(number = count) %>%
    select(year, step, area, length, number) %>%
    group_by(year, step, area, length) %>%
    summarize(number = sum(number)) %>%
    structure(area = mfdb_group(allareas = allareas),
              length = al.lengths)