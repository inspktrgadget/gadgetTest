# defaults for time, species, area

gd <- gadget_directory('gadgetTest/zbraModel')
gs.data <- gadget_directory('gadgetTest/zbraInit')
species.name <- 'zbra'
stocknames <- 'zbra'

st.year <- 1948
end.year <- 2013
data.st.year <- 1948
year.range <- st.year:end.year

defaults <- list(   
    areacell = mfdb_group("1" = 1),
    timestep = mfdb_timestep_quarterly,
    year = st.year:end.year,
    data.years = data.st.year:end.year,
    species = 'ZBRA')

timestep <- 
    unlist(defaults$timestep) %>%
    sort() %>% names() %>%
    substr(., 1,1) %>%
    as.numeric()