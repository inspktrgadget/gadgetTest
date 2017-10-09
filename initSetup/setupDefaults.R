# defaults for time, species, area

gd <- gadget_directory('gadgetTest/zbraModel')
species.name <- 'zbra'
stocknames <- 'zbra'

areas <- read.csv('atlantis/atlantisInfo/boxInfo.csv', header=T)
#boxes <- filter(areas, boundary == 0)$box_id
boxes <- sprintf("Box%s", filter(areas, boundary == 0)$box_id)

st.year <- 1940
end.year <- 2013
data.st.year <- 1948
year.range <- st.year:end.year
mfdb_timestep_monthly <-
    mfdb_group('1'=1,'2'=2,'3'=3,'4'=4,'5'=5,'6'=6,
               '7'=7,'8'=8,'9'=9,'10'=10,'11'=11,'12'=12)
defaults <- list(   
    areacell = mfdb_group("1" = boxes),
    timestep = mfdb_timestep_monthly,
    year = st.year:end.year,
    data.years = data.st.year:end.year,
    species = 'COD')
