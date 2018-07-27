# the is code to "gadgetize" the initial model performed as part of the mcsa
# this code re-configures the model based on the gadget output
# the mcsa will then be automated after this point as gadget output is always the same

library(tidyverse)
library(Rgadget)
fun_dir <- "~/gadget/models/functions/%s"
source(sprintf(fun_dir, "mcsa_functions.R"))

base_dir <- "~/gadget/models/gadgetTest"
setwd(base_dir)

# define names for directories
setup.d <- "setup/mcSetup"
source(sprintf("%s/defineDirs.R", setup.d))

# fit initial model to get gadget output and sample it to use as data in new model
source(sprintf("%s/fitGadgetMod.R", setup.d))

# copy files over to new model
source(sprintf("%s/copyGadgetModel.R", setup.d))

# pull datafiles from initMod 
source(sprintf("%s/mergeDataFiles.R", setup.d))

# copy over mfrun.R file
file.copy(sprintf("%s/mfrun.R", setup.d), sprintf("%s/mfrun.R", gd), overwrite = TRUE)
