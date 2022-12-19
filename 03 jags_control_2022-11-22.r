# script to fit Michaelis-Menten models in JAGS

library(rjags)
library(R2jags)

# testing models (TRUE) or running models for inference (FALSE)?
model.testing <- TRUE

# select y variable
## 1 = total species richness
## 2 = PIF species richness
## 3 = forest species richness
use.y <- 3

# select model(s) to run
models.to.run <- 1:28

# MCMC parameters
n.chains <- 3
if(model.testing){
    n.iter   <- 500
    n.burnin <- 100
    n.thin   <- 20
} else {
    # 2022-12-16: Double checked that same as mammal manuscript
	n.iter    <- 2e5
    n.burnin  <- 4e4
    n.thin    <- 100
}

# top-level directory that contains data and scripts
# ouputs will go in a subfolder
data.dir <- "C:/Users/ngreen62/OneDrive - Kennesaw State University/research"
data.dir <- paste(data.dir, "nrda/bird/indiana_bird_for_nick", sep="/")

# script with model definitions (data.dir)
model.name <- "04 model_code_2022-11-22.r"

# data file (data.dir)
data.name <- "bird_data_org_2022-12-09.csv"

# user input stops here! #######################################################

# define folder names that contain model code (models) and outputs (output)
model.dir <- paste(data.dir, "models", sep="/")
out.dir <- paste(data.dir, "output", sep="/")

# create model and output folders if they don't exist already
if(!dir.exists(model.dir)){dir.create(model.dir)}
if(!dir.exists(out.dir)){dir.create(out.dir)}

# source script with model definitions
model.path <- paste(data.dir, model.name, sep="/")
source(model.path)

# get data
in.path <- paste(data.dir, data.name, sep="/")
dat <- read.csv(in.path, header=TRUE)
# cut down dataset for testing (saves time/memory)
if(model.testing){dat <- dat[which(dat$repl <= 5),]}

# model definitions below
# for each response variable, 28 models can be fit with different 
# explanatory variables (nsamps or nsubs), response family (Poisson or NB),
# interactions with habitat diversity, or random effect of site.
# Models are divided into "groups", which have the same structure on the 
# righthand side. E.g., models 1 and 2 are from the same group, but can 
# form 6 runs: 3 response variables, with 2 x variables. 

#### Poisson models
## group A:
## level of effort only, no covariates, no random effect of site
# model 01: log(E(y)) ~ MM(nsamps); Poisson family
# model 02: log(E(y)) ~ MM(nsubs); Poisson family

## group B:
## level of effort + habitat interaction with alpha
# model 03: log(E(y)) ~ MM(nsamps, a*habitat); Poisson family
# model 04: log(E(y)) ~ MM(nsubs, a*habitat); Poisson family

## group C:
## level of effort + habitat interaction with beta
# model 05: log(E(y)) ~ MM(nsamps, b*habitat); Poisson family
# model 06: log(E(y)) ~ MM(nsubs, b*habitat); Poisson family

## group D:
## level of effort + habitat interaction with alpha and beta
# model 07: log(E(y)) ~ MM(nsamps, (a,b)*habitat); Poisson family
# model 08: log(E(y)) ~ MM(nsubs, (a,b)*habitat); Poisson family

## group E:
## level of effort + random effect of site on alpha
# model 09: log(E(y)) ~ MM(nsamps, a|site); Poisson family
# model 10: log(E(y)) ~ MM(nsubs, a|site); Poisson family

## group F:
## level of effort + random effect of site on beta
# model 11: log(E(y)) ~ MM(nsamps, b|site); Poisson family
# model 12: log(E(y)) ~ MM(nsubs, b|site); Poisson family

## group G:
## level of effort + random effect of site on alpha, beta
# model 13: log(E(y)) ~ MM(nsamps, (a,b)|site); Poisson family
# model 14: log(E(y)) ~ MM(nsubs, (a,b)|site); Poisson family

#### Negative binomial models
## group H:
## level of effort only, no covariates, no random effect of site
# model 15: log(E(y)) ~ MM(nsamps); Negative binomial family
# model 16: log(E(y)) ~ MM(nsubs); Negative binomial family

## group I:
## level of effort + habitat interaction with alpha
# model 17: log(E(y)) ~ MM(nsamps, a*habitat); Negative binomial family
# model 18: log(E(y)) ~ MM(nsubs, a*habitat); Negative binomial family

## group J:
## level of effort + habitat interaction with beta
# model 19: log(E(y)) ~ MM(nsamps, b*habitat); Negative binomial family
# model 20: log(E(y)) ~ MM(nsubs, b*habitat); Negative binomial family

## group K:
## level of effort + habitat interaction with alpha and beta
# model 21: log(E(y)) ~ MM(nsamps, (a,b)*habitat); Negative binomial family
# model 22: log(E(y)) ~ MM(nsubs, (a,b)*habitat); Negative binomial family

## group L:
## level of effort + random effect of site on alpha
# model 23: log(E(y)) ~ MM(nsamps, a|site); Negative binomial family
# model 24: log(E(y)) ~ MM(nsubs, a|site); Negative binomial family

## group M:
## level of effort + random effect of site on beta
# model 25: log(E(y)) ~ MM(nsamps, b|site); Negative binomial family
# model 26: log(E(y)) ~ MM(nsubs, b|site); Negative binomial family

## group N:
## level of effort + random effect of site on alpha, beta
# model 27: log(E(y)) ~ MM(nsamps, (a,b)|site); Negative binomial family
# model 28: log(E(y)) ~ MM(nsubs, (a,b)|site); Negative binomial family

## data preparation for model runs

# generate habitat variable in dataset
dat$habx <- 0
dat$habx[which(dat$hab_grass == 1 & dat$hab_woody == 0)] <- 1
dat$habx[which(dat$hab_grass == 0 & dat$hab_woody == 1)] <- 2
dat$habx[which(dat$hab_grass == 1 & dat$hab_woody == 1)] <- 3

# must be 0:
length(which(dat$habx == 0))

# generate model runs
run.df <- expand.grid(y=use.y, mod=models.to.run)
run.df$z <- LETTERS[rep(1:28, each=2)[run.df$mod]]
nruns <- nrow(run.df)

i <- 1

for(i in 1:nruns){
    
	imod <- run.df$mod[i]

    cat("Starting model", i ,"of", nruns, "in this run!\n")
    flush.console()	
	
	# number of factor levels depends on model
	use.nfac <- numeric(1)
	
	# y variable
    jags.dat <- list(y=dat[,grep("rich", names(dat))[run.df$y[i]]])
	
	# sample size N
	jags.dat$N <- nrow(dat)
	
	# continuous x variable (nsamps or nsubs)
	# odd == nsamps, even == nsubs
    if(imod %% 2 == 1){
        jags.dat$x1 <- dat$nsamps
    } else {
        jags.dat$x1 <- dat$nsubs
    }#else
	
	# habitat as fixed effect (if needed)
    if(imod %in% c(3:8, 17:22)){
	    jags.dat$xfac <- dat$habx
		use.nfac <- length(unique(dat$habx))
		jags.dat$nfac <- use.nfac
	}#if
	
	# site as random effect (if needed)
    if(imod %in% c(9:14, 23:28)){
	    jags.dat$xfac <- dat$site	
		use.nfac <- length(unique(dat$site))
		jags.dat$nfac <- use.nfac
	}#if
	
	# run name
	mod.00 <- formatC(imod, width=2, flag="0")
	run.name <- paste(paste0("y", use.y), mod.00, Sys.Date(), sep="_")
	run.name <- paste0(run.name, ".RData")

    # model file
    mod.name <- paste0("model", "_", run.df$z[i], ".txt")
	mod.path <- paste(model.dir, mod.name, sep="/")

    # initial values function
	## C = number of chains (== n.chains (global))
	## M = model number (== imod (global))
	## NF = number of factor levels (==jags.dat$nfac (global))
	inits.fun <- function(CH, M, NF){
	    res <- vector("list", length=CH)
        for(j in 1:CH){
		    # Group A: models 1-2
			if(M %in% c(1:2, 15:16)){
			    res[[j]]$A <- runif(1, 0.001, 5)
			    res[[j]]$B <- runif(1, 0.001, 5)
			}# models 1-2
			
		    # Group B: models 3-4
			if(M %in% c(3:4, 17:18)){
			    res[[j]]$A <- runif(NF, 0.001, 5)
			    res[[j]]$B <- runif(1, 0.001, 5)
			}# models 3-4

		    # Group C: models 5-6
			if(M %in% c(5:6, 19:20)){
			    res[[j]]$A <- runif(1, 0.001, 5)
			    res[[j]]$B <- runif(NF, 0.001, 5)
			}# models 5-6

		    # Group D: models 7-8
			if(M %in% c(7:8, 21:22)){
			    res[[j]]$A <- runif(NF, 0.001, 5)
			    res[[j]]$B <- runif(NF, 0.001, 5)
			}# models 7-8

		    # Group E: models 9-10
			if(M %in% c(9:10, 23:24)){
			    res[[j]]$mu.A <- runif(1, 0.001, 5)
				res[[j]]$sigma.A <- runif(1, 0.1, 10)
				res[[j]]$A <- rnorm(NF, res[[j]]$mu.A, res[[j]]$sigma.A)
			    res[[j]]$B <- runif(1, 0.001, 5)
			}# models 9-10

		    # Group F: models 11-12
			if(M %in% c(11:12, 25:26)){
			    res[[j]]$mu.B <- runif(1, 0.001, 5)
				res[[j]]$sigma.B <- runif(1, 0.1, 10)
				res[[j]]$B <- rnorm(NF, res[[j]]$mu.B, res[[j]]$sigma.B)
			    res[[j]]$A <- runif(1, 0.001, 5)
			}# models 11-12

		    # Group G: models 13-14
			if(M %in% c(13:14, 27:28)){
			    res[[j]]$mu.A <- runif(1, 0.001, 5)
				res[[j]]$sigma.A <- runif(1, 0.1, 10)
				res[[j]]$A <- rnorm(NF, res[[j]]$mu.A, res[[j]]$sigma.A)
			    res[[j]]$mu.B <- runif(1, 0.001, 5)
				res[[j]]$sigma.B <- runif(1, 0.1, 10)
				res[[j]]$B <- rnorm(NF, res[[j]]$mu.B, res[[j]]$sigma.B)
			}# models 13-14

            # Groups H - N: same as A - G,
			#    but also has overdispersion 
			#    term r for negative binomial
            if (M %in% 15:28){
			    res[[j]]$r <- runif(1, 0.001, 50)
			}#if models 15:28

			
		}#j
	    return(res)
	}#function
	
	use.inits <- inits.fun(n.chains, imod, use.nfac)
	
	# parameters to monitor
	params <- c("A", "B")
	if(imod %in% c(9, 10, 13, 14, 23, 24, 27, 28)){
	    params <- c(params, "mu.A", "sigma.A")
	}#if
	if(imod %in% c(11, 12, 13, 14, 25, 26, 27, 28)){
	    params <- c(params, "mu.B", "sigma.B")
	}#if
	if(imod %in% 15:28){
        params <- c(params, "r")	
	}#if
	
    # remove previous result if there is one
    if(exists("result")){remove(result)}
	
    # run model with JAGS
	result <- jags(
	    data=jags.dat, inits=use.inits, 
		parameters.to.save=params,
		model.file=mod.path,
	    n.chains=n.chains, n.iter=n.iter,
        n.burnin=n.burnin, n.thin=n.thin)#jags
	
	# write out result in its own workspace
	# in folder out.dir
	out.path <- paste(out.dir, run.name, sep="/")
	save(result, file=out.path)
	
	#print(result)
	
	cat("Model", i ,"of", nruns, "in this run complete!\n\n\n")
    flush.console()	
}#i for models to run

# end script!