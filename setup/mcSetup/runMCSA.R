# script to run what I am calling a cascading stock assessment
# this is a method where a stock assessment is performed using real data
# and then run n number of times again using output from the previous stock assessment
# it forms a sort of markov chain of stock assessments

library(tidyverse)
library(Rgadget)
rscript_dir <- "~/R/Rscripts/functions/%s"
source(sprintf(rscript_dir, "copy_from_dir.R"))
source(sprintf(rscript_dir, "diffmean.R"))
source(sprintf(rscript_dir, "expand_suitability.R"))
source(sprintf(rscript_dir, "add_lengthgroups.R"))
source(sprintf(rscript_dir, "survey_gadget.R"))
source(sprintf(rscript_dir, "detect_fleet_likelihood.R"))

base_dir <- "~/gadget/models/gadgetTest/gadTestMod"
setwd(base_dir)


# establish length of chain
n <- 5

# run loop of cascading stock assessments
lapply(1:n, function(x) {
    cat("Running iteration", x)
    setwd(base_dir)
    iter_dir <- "iterMod_%s"
    if (x == 1) {
        # for the first iteration setup the model using gadgetizeModel.R
        setwd(sprintf(iter_dir, x))
        source("mfrun.R")
        setwd(base_dir)
    } else {
        # fit model to get the data, then we will proceed with model setup
        setwd(sprintf(iter_dir, (x-1)))
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
        stock_data <- 
            add_lengthgroups(fit$stock.std, lg) %>%
            survey_gadget(length_group = lg, suit_data = sample_selectivity, survey_sd = 0) %>%
            filter(number > 0)
        mainfile <- read.gadget.main()
        stockfile <- Rgadget:::read.gadget.stockfiles(mainfile$stockfiles)
        dl <- stockfile$cod@dl
        setwd(base_dir)
        
        # define some directories as names to reduce writing
        dat_dir <- paste(sprintf(iter_dir, x), "Data", sep = "/")
        agg_dir <- paste(sprintf(iter_dir, x), "Aggfiles", sep = "/")
        mod_dir <- paste(sprintf(iter_dir, x), "Modelfiles", sep = "/")
        
        # copy files over
        copy_from_dir(sprintf(iter_dir, (x-1)), sprintf(iter_dir, x), recursive = TRUE)
        unlink(paste(sprintf(iter_dir, x), "WGTS", sep = "/"), recursive = TRUE)
        # code to read in data files and swap old data with new
        datfile_names <- grep("^fleet|^bounds", dir(dat_dir), value = TRUE, invert = TRUE)
        fleets <- unique(stock_data$fleet)
        dl_lengrps <- seq(min(stock_data$length), max(stock_data$length), by = dl)
        si_lengrps <- 
            datfile_names %>%
            strsplit("\\.") %>%
            unlist() %>% as.numeric() %>% unique() %>% na.omit()
        
        # put data from previous iterations output into this iterations model
        initmod_datafiles <- 
            lapply(datfile_names, function(x) {
                dat <- read.table(paste(dat_dir, x, sep = "/"), 
                                  comment.char = ";",
                                  stringsAsFactors = FALSE)
                if (grepl("catchdistribution", x)) {
                    names(dat) <- c("year", "step", "area", "age", "length", "number")
                    # subset fleet from stock_data
                    fleet_data <- 
                        stock_data %>%
                        filter(fleet == detect_fleet_likelihood(fleets, x)) %>%
                        select(-stock, -fleet) %>%
                        mutate(lengrp = dl_lengrps[findInterval(length, dl_lengrps)])
                    if (grepl("aldist", x)) {
                        # prep fleet data to likelihood type
                        aldist_fleet_data <-
                            fleet_data %>%
                            group_by(year, step, area, age, lengrp) %>%
                            summarize(number = sum(number)) %>%
                            ungroup() %>%
                            mutate(age = paste0("age", age), 
                                   length = paste0("len", lengrp),
                                   area = "all")
                        # merge in with the old dataset
                        newdat <- 
                            dat %>%
                            select(-age, -length, -number) %>%
                            unique() %>%
                            left_join(aldist_fleet_data) %>%
                            select(year, step, area, age, length, number)
                        write.table(newdat, 
                                    file = paste(dat_dir, x, sep = "/"), sep = "\t",
                                    quote = FALSE, col.names = FALSE, row.names = FALSE)
                        age_aggfile_name <- gsub("sumofsquares", "age.agg", x)
                        write.table(data.frame(paste0("age", unique(fleet_data$age)), 
                                               unique(fleet_data$age)),
                                    file = paste(agg_dir, age_aggfile_name, sep = "/"),
                                    sep = "\t", quote = FALSE, 
                                    col.names = FALSE, row.names = FALSE)
                    } else if (grepl("ldist", x)) {
                        # prep fleet data to likelihood type
                        ldist_fleet_data <- 
                            fleet_data %>%
                            group_by(year, step, area, lengrp) %>%
                            summarize(number = sum(number)) %>%
                            ungroup() %>%
                            mutate(age = "all",
                                   length = paste0("len", lengrp),
                                   area = "all")
                        # merge in with the old dataset
                        newdat <- 
                            dat %>%
                            select(-age, -length, -number) %>%
                            unique() %>%
                            left_join(ldist_fleet_data) %>%
                            select(year, step, area, age, length, number)
                        write.table(newdat,
                                    file = paste(dat_dir, x, sep = "/"), sep = "\t",
                                    quote = FALSE, col.names = FALSE, row.names = FALSE)
                        age_aggfile_name <- gsub("sumofsquares", "age.agg", x)
                        write.table(data.frame("all", t(unique(fleet_data$age))),
                                    file = paste(agg_dir, age_aggfile_name, sep = "/"),
                                    sep = "\t", quote = FALSE, col.names = FALSE, row.names = FALSE)
                    }
                } else if (grepl("surveyindices", x)) {
                    names(dat) <- c("year", "step", "area", "length", "number")
                    # subset fleet from stock_data
                    fleet_data <- 
                        stock_data %>%
                        filter(fleet == detect_fleet_likelihood(fleets, x)) %>%
                        select(-stock, -fleet) %>%
                        mutate(lengrp = si_lengrps[findInterval(length, si_lengrps)]) %>%
                        group_by(year, step, area, lengrp) %>%
                        summarize(number = sum(number)) %>%
                        ungroup() %>%
                        mutate(area = "all",
                               length = paste0("len", lengrp))
                    newdat <- 
                        dat %>%
                        select(-number) %>%
                        left_join(fleet_data) %>%
                        select(year, step, area, length, number)
                    write.table(newdat,
                                file = paste(dat_dir, x, sep = "/"),
                                quote = FALSE, row.names = FALSE, col.names = FALSE)
                }
            })
        setwd(sprintf(iter_dir, x))
        source("mfrun.R")
        setwd(base_dir)
    }
})