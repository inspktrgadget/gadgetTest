# script to copy model from initMod over to new model

# copy files over
copy_from_dir(initmod_dir, gd, recursive = TRUE)
unlink(paste(gd, "WGTS", sep = "/"), recursive = TRUE)
