# find some decent starting values for recl and stddev

init.sigma <- 
    std %>%
    group_by(age) %>%
    summarize(ml = mean(mean.length),
              ms = mean(length.sd))


lw.tmp <-
    std %>%
    nls(mean.weight ~ a*mean.length^b,.,start=list(a=0.000001,b=3)) %>%
    coefficients() %>%
    as.numeric()

## populate the model with starting default values

## alpha and beta for lw relationship
weight.alpha <- lw.tmp[1] #0.0000021
weight.beta <- lw.tmp[2] #3.3437

## setup M and determine initial abundance
#source('cod/modelCheck/getAtlantisMort.R')
#nat.mort <- round(m.data$m, 3)
#rc <- 20

# age.mean.formula <- 'exp(-1*(%2$s.M+%3$s.init.F)*%1$s)*%2$s.init.%1$s'
rec.number <- sprintf('%1$s.rec.scalar*%1$s.rec.%2$s', species.name, year.range)
rec.sd <- sprintf('#%s.rec.sd', species.name)

## set up the one stock stock
zbra <- 
    gadgetstock('zbra', gd$dir, missingOkay=T) %>%
    gadget_update('stock',
                  minage = 0,
                  maxage = 19,
                  minlength = 1,
                  maxlength = 200,
                  dl = 3,
                  livesonareas = 1) %>%
    gadget_update('doesgrow',
                  growthparameters=c(linf=sprintf('#%s.linf', species.name), 
                                     k=sprintf('#%s.k', species.name),
                                     alpha=weight.alpha,
                                     beta=weight.beta),
                  beta=to.gadget.formulae(quote(10*zbra.bbin))) %>%
    gadget_update('naturalmortality', 
                  m.estimate.formula(age=.[[1]]$minage:.[[1]]$maxage,
                                     m=sprintf('%s.m.decay', .[[1]]$stockname), 
                                     max.m=sprintf('%s.max.m', .[[1]]$stockname),
                                     min.m=sprintf('%s.min.m', .[[1]]$stockname))) %>%
    gadget_update('initialconditions',
                  normalparam=
                      data_frame(age = .[[1]]$minage:.[[1]]$maxage, 
                                 area = 1,
                                 age.factor=init.age.factor(age,
                                                            sprintf('%s.init.decay', .[[1]]$stockname),
                                                            sprintf('%s.init.scalar', .[[1]]$stockname),
                                                            sprintf('%s.init.min', .[[1]]$stockname)),
                                 area.factor=sprintf('( * #%1$s.mult #%1$s.init.abund)',
                                                     .[[1]]$stockname),
                                 mean = vonb_formula(.[[1]]$minage:.[[1]]$maxage,
                                                     linf=sprintf('%s.linf', species.name),
                                                     k=sprintf('%s.k', species.name),
                                                     recl=sprintf('%s.recl', species.name)),
                                 stddev = c(init.sigma$ms, 
                                            rep(init.sigma$ms[nrow(init.sigma)],
                                                (.[[1]]$maxage-.[[1]]$minage)-(nrow(init.sigma)-1))),
                                 alpha = weight.alpha,
                                 beta = weight.beta)) %>%
    gadget_update('refweight',
                  data=data_frame(length=seq(.[[1]]$minlength,
                                             .[[1]]$maxlength,
                                             .[[1]]$dl),
                                  mean = weight.alpha*length^weight.beta)) %>%
    gadget_update('iseaten', 1) %>%
    # gadget_update('doesmature', 
    #               maturityfunction = 'continuous',
    #               maturestocksandratios = sprintf('%smat 1',species_name),
    #               coefficients = sprintf('( * 0.001 #%1$s.mat1) #%1$s.mat2 0 0',
    #                                         species_name)) %>% 
    # gadget_update('doesmove',
    #               transitionstocksandratios = sprintf('%s.mat 1', species.name),
    #               transitionstep = 4) %>%
    gadget_update('doesrenew',
                  normalparam = 
                      data_frame(
                          year = year.range,
                          step = 1,
                          area = 1, 
                          age = .[[1]]$minage,
                          number = parse(text=rec.number) %>%
                                   map(to.gadget.formulae) %>%
                                   unlist(),
                          mean = vonb_formula(
                                    age=.[[1]]$minage,
                                    linf=sprintf('%s.linf', species.name),
                                    k=sprintf('%s.k', species.name),
                                    recl=sprintf('%s.recl', species.name)),
                          stddev = rec.sd,
                          alpha = weight.alpha,
                          beta = weight.beta))



write.gadget.file(zbra, gd$dir)
