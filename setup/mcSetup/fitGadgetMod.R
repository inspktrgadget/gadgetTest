# fit model to get the data, then we will proceed with model setup
setwd(initmod_dir)
fit <- gadget.fit(printfile.printatstart = 0,
                  printfile.steps = "all",
                  rec.len.param = TRUE)
lg <- seq(0.5, max(fit$suitability$length), by = 1)
lg_means <- diffmean(lg)
selectivity <- 
    fit$suitability %>%
    filter(suit > 0) %>%
    group_by(stock, fleet, length) %>%
    summarize(suit = mean(suit))
sampling_prop <- 0.01
sample_selectivity <- 
    expand_suitability(selectivity, lg_means) %>%
    mutate(suit = suit * sampling_prop)
const_selectivity <- 
    expand.grid(stock = unique(fit$stock.std$stock),
                fleet = unique(fit$catchdist.fleets$fleetnames),
                length = lg_means,
                suit = 1,
                stringsAsFactors = FALSE)
stock_data <- 
    add_lengthgroups(fit$stock.std, lg) %>%
    survey_gadget(length_group = lg, suit_data = const_selectivity, survey_sd = 0) %>%
    filter(number > 0)
mainfile <- read.gadget.main()
stockfile <- Rgadget:::read.gadget.stockfiles(mainfile$stockfiles)
dl <- stockfile$cod@dl
setwd(base_dir)
