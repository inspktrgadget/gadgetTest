## code to compare gadget model to gadget model
library(mfdb)
setwd('~/gadget/models/')

## source output from initial model
gs.data <- gadget_directory('gadgetTest/zbraInit')
source('gadgetTest/zbraSetup/getSimData.R')

## format biomass data and plot
op.model.biomass <- 
    std %>%
    filter(month == 3) %>%
    group_by(year) %>%
    summarize(total.biomass = sum(number*mean.weight))

biomass.comp.plot <- 
    ggplot(data=op.model.biomass, 
       aes(x=year, y=total.biomass/1e6, color='Op. Model')) + 
    geom_line() + 
    geom_line(data=fit$res.by.year,
              aes(x=year, y=total.biomass/1e6, color='Est. Model')) + 
    theme_bw() + xlab('Year') + ylab('Biomass (thousands of tons)') + 
    scale_color_manual('', breaks=c('Op. Model', 'Est. Model'), 
                       values=c('red', 'black'))
    

## format numbers and plot
op.model.numbers <- 
    std %>%
    filter(month == 3) %>%
    group_by(year) %>%
    summarize(total.number = sum(number))

numbers.comp.plot <- 
    ggplot(data=op.model.numbers, 
           aes(x=year, y=total.number/1e6, color='Op. Model')) + 
    geom_line() + 
    geom_line(data=fit$res.by.year, 
              aes(x=year, y=total.number/1e6, color='Est. Model')) + 
    theme_bw() + xlab('Year') + ylab('Numbers (millions)') + 
    scale_color_manual('', breaks=c('Op. Model', 'Est. Model'), 
                       values=c('red', 'black'))



## format numbers by age and plot
op.model.age.numbers <- 
    std %>%
    filter(month == 3) %>%
    group_by(year, age) %>%
    summarize(total.number = sum(number))

est.model.age.numbers <- 
    fit$stock.std %>%
    group_by(year, age) %>%
    summarize(total.number = sum(number))

age.numbers.comp.plot <- 
    ggplot(data=op.model.age.numbers, 
           aes(x=year, y=total.number/1e6, color='Op. Model')) + 
    geom_line() + 
    geom_line(data=est.model.age.numbers, 
              aes(x=year, y=total.number/1e6, color='Est. Model')) + 
    facet_wrap(~age) + 
    theme_bw() + xlab('Year') + ylab('Numbers (millions)') + 
    scale_color_manual('', breaks=c('Op. Model', 'Est. Model'), 
                       values=c('red', 'black'))




