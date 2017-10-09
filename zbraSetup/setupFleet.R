# get landings from zbraInit model (should be same as cod in atlantis)

comm.landings <- 
	filter(prey, year >= 1948) %>%
	select(year, month, area, age, biomass.consumed) %>%
	mutate(area = as.numeric(as.character(gsub('area', '', area)))) %>%
	group_by(year, month) %>%
	summarize(weight = sum(biomass.consumed)) %>%
    rename(step = month) %>%
    mutate(area = 1) %>% 
    select(year, step, area, weight)

comm.landings <- structure(as.data.frame(comm.landings), 
                           area_group=mfdb_group(`1` = 1))

## set up and make surveys as fleet
# igfs.landings <- structure(data.frame(year=defaults$data.years, step=2, area=1, number=1),
#                            area_group=mfdb_group(`1` = 1))
# aut.landings <- structure(data.frame(year=defaults$data.years, step=4, area=1, number=1),
#           area_group=mfdb_group(`1` = 1))


gadgetfleet('Modelfiles/fleet', gd$dir, missingOkay=T) %>%
    # gadget_update('totalfleet',
    #               name = 'spr',
    #               suitability = fleet.suit('spr', stocknames, 'constant'),
    #               data=igfs.landings) %>%
    # gadget_update('totalfleet',
    #               name = 'aut', 
    #               suitability = fleet.suit('aut', stocknames, 'constant'),
    #               data = aut.landings) %>%
    gadget_update('totalfleet',
                  name = 'comm',
                  suitability = fleet.suit('comm', stocknames, 'exponentiall50'),
                  data = comm.landings) %>%
    # gadget_update('totalfleet',
    #               name = 'discards',
    #               suitability = fleet.suit('discards', stocknames, 'exponentiall50'),
    #               data = discards[[1]]) %>%
    write.gadget.file(gd$dir)
