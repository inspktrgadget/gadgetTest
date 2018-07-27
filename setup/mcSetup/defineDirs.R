# setup some useful shortcut names for directories
if (!file.exists("gadTestMod/initMod")) {
    wd <- getwd()
    setwd("gadTestMod")
    dir.create("initMod")
    system("mv * initMod")
    setwd(wd)
}
initmod_dir <- "gadTestMod/initMod"
gd <- "gadTestMod/iterMod_1"
dat_dir <- paste(gd, "Data", sep = "/")
agg_dir <- paste(gd, "Aggfiles", sep = "/")
mod_dir <- paste(gd, "Modelfiles", sep = "/")
initmod_data <- paste(initmod_dir, "Data", sep = "/")
initmod_modfiles <- paste(initmod_dir, "Modelfiles", sep = "/")