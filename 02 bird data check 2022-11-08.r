# script to fit Michaelis-Menten models in JAGS

# top-level directory that contains data; will contain outputs
data.dir <- "C:/Users/ngreen62/OneDrive - Kennesaw State University/research"
data.dir <- paste(data.dir, "nrda/bird/indiana_bird_for_nick", sep="/")

# define folder names that contain model code (models) and outputs (output)
model.dir <- paste(data.dir, "models", sep="/")
out.dir <- paste(data.dir, "output", sep="/")

# create model and output folders if they don't exist already
if(!dir.exists(model.dir)){dir.create(model.dir)}
if(!dir.exists(out.dir)){dir.create(out.dir)}

# get data
in.name <- "bird_data_org_2022-11-08.csv"
in.path <- paste(data.dir, in.name, sep="/")
dat <- read.csv(in.path, header=TRUE)

# check species richness by site
par(mfrow=c(1,3), mar=c(5.1, 5.1, 2.1, 1.1), lend=1, las=1, bty="n",
    cex.axis=1.3, cex.lab=1.3)
boxplot(rich.tot~site, data=dat, xaxt="n", xlab="Site",
    ylab="Total species richness (S)", ylim=c(0, 70))
title(main="Total species richness", adj=0, cex.main=2)
axis(side=1, at=1:4, labels=c("Bell", "Bluffton", "Deetz", "Holden"))
boxplot(rich.pif~site, data=dat, xaxt="n", xlab="Site",
    ylab="PIF species richness (S)", ylim=c(0, 16))
title(main="PIF species richness", adj=0, cex.main=2)
axis(side=1, at=1:4, labels=c("Bell", "Bluffton", "Deetz", "Holden"))
boxplot(rich.for~site, data=dat, xaxt="n", xlab="Site",
    ylab="Forest species richness (S)", ylim=c(0, 16))
title(main="Forest species richness", adj=0, cex.main=2)
axis(side=1, at=1:4, labels=c("Bell", "Bluffton", "Deetz", "Holden"))

# confound between habitat and site?
dat$habx <- paste(dat$hab_woody, dat$hab_grass, sep="_")
sort(unique(dat$habx))	
# "0_1" "1_0" "1_1"
ftable(habx~site, data=dat)

# shows that:
# Site 1 (Bell): all simulations have all "forest but no grass" points
# Site 2 (Bluffton): has simulations with all grass (0_1), all forest (1_0),
#                    and grass and forest (1_1)
# Site 3 (Deetz): all simulations have all "forest but no grass" points
# Site 4 (Holden): has simulations with all grass (0_1), all forest (1_0),
#                  and grass and forest (1_1)

#      habx   0_1   1_0   1_1
# site                       
# 1             0 13700     0
# 2           423   610  6167
# 3             0  4500     0
# 4           130   382  1688



par(mfrow=c(1,1))
mosaicplot(habx~site, data=dat)



# select y variable
# uncomment one of the following:
use.y <- dat$rich.tot
#use.y <- dat$rich.pif
#use.y <- dat$rich.for

# models to run:
# level of effort only, no covariates, no random effect of site
# model 01: log(E(y)) ~ MM(nsamps)
# model 02: log(E(y)) ~ MM(nsubs)

# level of effort + habitat, no random effect of site)
# model 03: log(E(y)) ~ MM(nsamps) + habitat
# model 04: log(E(y)) ~ MM(nsubs) + habitat

# level of effort, no covariates, random effect of site
# model 05: log(E(y)) ~ MM(nsamps, a|site)
# model 06: log(E(y)) ~ MM(nsamps, b|site)
# model 07: log(E(y)) ~ MM(nsamps, a,b|site)
# model 08: log(E(y)) ~ MM(nsubs, a|site)
# model 09: log(E(y)) ~ MM(nsubs, b|site)
# model 10: log(E(y)) ~ MM(nsubs, a,b|site)

# level of effort + habitat, random effect of site
# model 11: log(E(y)) ~ MM(nsamps, a|site) + habitat
# model 12: log(E(y)) ~ MM(nsamps, b|site) + habitat
# model 13: log(E(y)) ~ MM(nsamps, a,b|site) + habitat
# model 14: log(E(y)) ~ MM(nsubs, a|site) + habitat
# model 15: log(E(y)) ~ MM(nsubs, b|site) + habitat
# model 16: log(E(y)) ~ MM(nsubs, a,b|site) + habitat


par(mfrow=c(1,3))
plot(table(dat$rich.tot), main="All species", xlab="Species richness", xlim=c(0, 80))
plot(table(dat$rich.pif), main="PIF species", xlab="Species richness", xlim=c(0, 80))
plot(table(dat$rich.for), main="Forest species", xlab="Species richness", xlim=c(0, 80))

# count 0s relative to Poisson
count0 <- function(x){
    require(MASS)
    fx <- fitdistr(x, "Poisson")
    n <- length(x)
    # proportion of 0s expected under Poisson
    expect.0 <- dpois(0, fx$estimate)
    # proportion of 0s in data
    obs.0 <- length(which(x==0)) / n
    cat(obs.0, "0s observed out of", expect.0, "expected under Poisson\n")    
}
count0(dat$rich.tot)
count0(dat$rich.pif)
count0(dat$rich.for)


par(mfrow=c(1,3))
plot(table(dat$rich.tot), main="All species", xlab="Species richness", xlim=c(0, 80))
plot(table(dat$rich.pif), main="PIF species", xlab="Species richness", xlim=c(0, 80))
plot(table(dat$rich.for), main="Forest species", xlab="Species richness", xlim=c(0, 80))

var(dat$rich.tot)
var(dat$rich.pif)
var(dat$rich.for)
mean(dat$rich.tot)
mean(dat$rich.pif)
mean(dat$rich.for)
# count 0s relative to Poisson
count0 <- function(x){
    require(MASS)
    fx <- fitdistr(x, "Poisson")
    n <- length(x)
    # proportion of 0s expected under Poisson
    expect.0 <- dpois(0, fx$estimate)
    # proportion of 0s in data
    obs.0 <- length(which(x==0)) / n
    ratio.0 <- obs.0 / expect.0
    cat("Observed 0s =", length(which(x == 0)), "\n")
    cat("Observed proportion of 0s =", obs.0, "\n")
    cat("Expected 0s under Poisson =", round(expect.0*n, 1), "\n")
    cat("Expected p(0) under Poisson =", expect.0, "\n")
    cat("Ratio =", ratio.0, "\n")

    # proprotion expected under negative binomial
    fx <- suppressWarnings(fitdistr(x, "negative binomial"))
    expect.0nb <- dnbinom(0, size=fx$estimate[1], mu=fx$estimate[2])
    ratio.0nb <- obs.0 / expect.0nb
   
    cat("Observed proportion of 0s =", obs.0, "\n")
    cat("Expected 0s under negative binomial =", round(expect.0nb*n,1), "\n")
    cat("Expected p(0) under negative binomial =", expect.0nb, "\n")
    cat("Ratio =", ratio.0nb, "\n")
}
count0(dat$rich.tot)
count0(dat$rich.pif)
count0(dat$rich.for)



