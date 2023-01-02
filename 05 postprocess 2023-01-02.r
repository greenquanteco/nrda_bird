# postprocessing for NRDA bird models
# follows script 03 jags_control_2022-12-22.r
# and 04 model_code_2022-11-12.r
# data file: bird_data_org_2022-12-09.csv

library(R2jags)
library(rjags)

dat.dir <- "C:/Users/ngreen62/OneDrive - Kennesaw State University"
dat.dir <- paste(dat.dir, "research/nrda/bird/outputs 2023-01-02", sep="/")

files <- dir(dat.dir)
nfiles <- length(files)

nx <- strsplit(files, "_")
res <- data.frame(y=sapply(nx, "[", 1), mod=sapply(nx, "[", 2))
res$x <- c("samps", "subs")
res$params <- NA
res$conv <- NA
res$dic <- NA

# require R-hat < 1.01 for convergence
rhat.cutoff <- 1.01

# should be all 1s:
ftable(y~mod, data=res)

# bring in results and add diagnostics to res
for(i in 1:nfiles){
    ne <- new.env()
	in.name <- paste(dat.dir, files[i], sep="/")
	load(in.name, envir=ne)
	sm <- ne$result$BUGSoutput$summary
	
	res$params[i] <- nrow(sm)
	res$conv[i] <- length(which(sm[,"Rhat"] < 1.01))
	res$dic[i] <- ne$result$BUGSoutput$DIC
	rm(ne)
}#i

# don't want models where parameters didn't converge
res$check <- ifelse(res$conv < res$params, 0, 1)
res.df <- res[which(res$check==1),]
res.df$check <- NULL
res.df$conv <- NULL

# check: is there >=1 model for each combo of Y and X?
## yes
ftable(y~x, data=res.df)

# calculate DIC weights for each combo of y and x variable
res.df$yx <- paste(res.df$y, res.df$x, sep="_")
yxs <- sort(unique(res.df$yx))
nyx <- length(yxs)
res.df$delta <- NA
res.df$dicwt <- NA
res.df$rank <- NA

for(i in 1:nyx){
    flag <- which(res.df$yx == yxs[i])
	res.df$delta[flag] <- res.df$dic[flag] - min(res.df$dic[flag])
	res.df$dicwt[flag] <- exp(-0.5*res.df$delta[flag])
	res.df$dicwt[flag] <- res.df$dicwt[flag] / sum(res.df$dicwt[flag])
	res.df$rank[flag] <- rank(res.df$delta[flag])
}

# sort by y, then dic weight
res.df <- res.df[order(res.df$yx, res.df$delta),]
res.df

# grab top 3 models for each 
top3 <- res.df[which(res.df$rank <= 3),]

top3.list <- vector("list", length=nrow(top3))
names(top3.list) <- paste(top3$yx, top3$rank, sep="_")
for(i in 1:nrow(top3)){
    get.string <- paste(top3$y[i], top3$mod[i], sep="_")
	get.file <- files[grep(get.string, files)]
	get.path <- paste(dat.dir, get.file, sep="/")
	ne <- new.env()
    load(get.path, envir=ne)
	top3.list[[i]] <- ne$result
}#i

# prints out parameter tables for top 3 models 
# for each combo of Y and X
top3.list

# next step: make figures (waiting on revised outputs 2023-01-02)

# end script!