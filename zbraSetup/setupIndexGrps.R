# pre-baked length groupings and length attr for survey indices
# change if you get serious about this
# general length groupings for both spring and fall
short <- 0
mid <- 18
long <- 36
ind.groups <- list(short = c(short,mid),
                   mid = c(mid, long),
                   long = c(long, maxlen))
ind.lengths <- lapply(ind.groups, FUN=function(x) {
    mfdb_interval('len', x)
})

# length attr for spring
ss.lengths <- structure(list(len0 = c(short,mid)), min=short, max=mid)
sm.lengths <- structure(list(len18 = c(mid,long)), min=mid, max=long)
sl.lengths <- structure(list(len36 = c(long,maxlen)), min=long, max=maxlen)

# length attr for autumn
as.lengths <- structure(list(len0 = c(short,mid)), min=short, max=mid)
am.lengths <- structure(list(len18 = c(mid,long)), min=mid, max=long)
al.lengths <- structure(list(len36 = c(long,maxlen)), min=long, max=maxlen)
