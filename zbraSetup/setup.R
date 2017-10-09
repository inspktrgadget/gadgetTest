## general setup for gadget models
library(plyr)
library(dplyr)
library(mfdb)
library(Rgadget)


setwd('~/gadget/models')
source('functions/gadgetUtils.R')
setup.d <- 'gadgetTest/zbraSetup'

source(sprintf('%s/setupDefaults.R', setup.d))
source(sprintf('%s/setupTimeArea.R', setup.d))

# get the simulated data from zbraInit
source(sprintf('%s/getSimData.R', setup.d))


# update fleets and/or models
source(sprintf('%s/setupFleet.R', setup.d))
source(sprintf('%s/setupModel.R', setup.d))

# run gadget -s -log logfile.txt from terminal

# update params and likelihood components
source(sprintf('%s/setupParams.R', setup.d))

source(sprintf('%s/setupCatchdistribution.R', setup.d))
source(sprintf('%s/setupIndices.R', setup.d))
source(sprintf('%s/setupLikelihood.R', setup.d))

file.copy(sprintf('%s/run.R', setup.d), gd$dir)
file.copy(sprintf('%s/mfrun.R', setup.d), gd$dir)
file.copy(sprintf('%s/optinfofile', setup.d), gd$dir)

