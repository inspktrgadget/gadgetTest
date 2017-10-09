# setup time and area files

## write out area and time files
gadgetfile('time',
           file_type = 'time', 
           components = list(list(firstyear = st.year,
                                  firststep = 1,
                                  lastyear = end.year,
                                  laststep = 1,
                                  notimesteps = c(4, rep(3, 4))))) %>%
    write.gadget.file(gd$dir)

if (!dir.exists(paste(gd$dir, 'Modelfiles', sep='/'))) {
    dir.create(paste(getwd(), gd$dir, 'Modelfiles', sep='/'))
}
file.copy(paste(getwd(), gs.data$dir, 'Modelfiles/area', sep='/'),
          paste(getwd(), gd$dir, 'Modelfiles/area', sep='/'))

# amend the mainfile to include the areafile
mainfile <- read.gadget.main(paste(gd$dir, 'main', sep='/'))
mainfile$areafile <- 'Modelfiles/area'
write.gadget.main(mainfile, file=paste(gd$dir, 'main', sep='/'))
