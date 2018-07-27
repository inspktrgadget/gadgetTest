# code to read in data files and swap old data with new
datfile_names <- grep("^fleet|^bounds", dir(dat_dir), value = TRUE, invert = TRUE)
fleets <- unique(stock_data$fleet)
dl_lengrps <- seq(min(stock_data$length), max(stock_data$length), by = dl)
si_lengrps <- 
    datfile_names %>%
    strsplit("\\.") %>%
    unlist() %>% as.numeric() %>% unique() %>% na.omit()


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

gadgetfleet("Modelfiles/fleet", gd, missingOkay = FALSE) %>%
    update_suit("spr") %>%
    update_suit("aut") %>%
    write.gadget.file(gd)