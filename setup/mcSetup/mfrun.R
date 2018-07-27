gt <- system.time(
    tmp <- gadget.iterative(rew.sI=TRUE,
                            main="main",
                            grouping=list(`age0`=c("spr.si.0.17", "aut.si.0.17"),
					  `age1`=c("spr.si.17.31", "aut.si.17.31"),
					  `age2`=c("spr.si.31.45", "aut.si.31.45"),
					  `age3`=c("spr.si.45.55", "aut.si.45.55"),
					  `age4+`=c("spr.si.55.pl", "aut.si.55.pl")),
                            wgts="WGTS")
)[3]

print(paste("Iterative model took",
            c(gt %/% 3600),
            "hours and",
            c((gt - (gt %/% 3600) * 3600) %/% 60),
            "minutes to run."))

