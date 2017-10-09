# script to pull necessary data from zbraInit to
# setup model and likelihood components

std <- read.table(paste(getwd(), gs.data$dir, 'WGTS/out.fit/zbra.std',
                        sep='/'),
                  header=F, comment.char = ';')

names(std) <- c('year', 'month', 'area', 'age', 'number', 
                'mean.length', 'mean.weight', 'length.sd', 
                'number.consumed', 'biomass.consumed')

full <- read.table(paste(getwd(), gs.data$dir, 'WGTS/out.fit/zbra.full',
                        sep='/'),
                  header=F, comment.char = ';')

names(full) <- c('year', 'month', 'area', 'age', 
                'length', 'number', 'mean.weight')

prey <- read.table(paste(getwd(), gs.data$dir, 'WGTS/out.fit/zbra.prey',
                        sep='/'),
                  header=F, comment.char = ';')

names(prey) <- c('year', 'month', 'area', 'age', 'length', 
                 'number.consumed', 'biomass.consumed', 'mortality')