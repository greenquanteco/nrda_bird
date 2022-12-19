# script to define JAGS models
# called from jags_control_2022-xx-xx.R

## group A:
## level of effort only, no covariates, no random effect of site
# y ~ Poisson(lambda)
# log(lambda) = MM(x1)

sink(paste(model.dir, "model_A.txt", sep="/"))
cat("
model{

# priors
## MM terms
A ~ dunif(0, 5)
B ~ dunif(0, 5)

# likelihood
for(i in 1:N){
    # Poisson draw for response variable
	y[i] ~ dpois(lambda.rich[i])
	
	# expected value
	lambda1[i] <- (A * x1[i])/(B + x1[i])
	
	# stabilize the log transformation
	# max = 5; exp(5) ~= 148, way more than ever observed
	# min = -4; exp(-4) ~= 0.02, <<1 expected, way less than ever observed
	lambda2[i] <- max(-4, min(5, lambda1[i]))
	lambda.rich[i] <- exp(lambda2[i])
}#i
# 
}#model
", fill=TRUE)# closes cat
sink() # for model A

## group B:
## level of effort + habitat interaction with alpha
# y ~ Poisson(lambda)
# log(lambda) = (A[xfac]*x1)/(B+x1)
sink(paste(model.dir, "model_B.txt", sep="/"))
cat("
model{

# priors
## MM terms
for(i in 1:nfac){
    A[i] ~ dunif(0, 5)
}#i

B ~ dunif(0, 5)

# likelihood
for(i in 1:N){
    # Poisson draw for response variable
	y[i] ~ dpois(lambda.rich[i])
	
	# expected value
	lambda1[i] <- (A[xfac[i]] * x1[i])/(B + x1[i])
	
	# stabilize the log transformation
	# max = 5; exp(5) ~= 148, way more than ever observed
	# min = -4; exp(-4) ~= 0.02, <<1 expected, way less than ever observed
	lambda2[i] <- max(-4, min(5, lambda1[i]))
	lambda.rich[i] <- exp(lambda2[i])
}#i
# 
}#model
", fill=TRUE)# closes cat
sink() # for model B


## group C:
## level of effort + habitat interaction with beta
# y ~ Poisson(lambda)
# log(lambda) = (A*x1)/(B[xfac]+x1)
sink(paste(model.dir, "model_C.txt", sep="/"))
cat("
model{

# priors
## MM terms
A ~ dunif(0, 5)
for(i in 1:nfac){
    B[i] ~ dunif(0, 5)
}#i

# likelihood
for(i in 1:N){
    # Poisson draw for response variable
	y[i] ~ dpois(lambda.rich[i])
	
	# expected value
	lambda1[i] <- (A * x1[i])/(B[xfac[i]] + x1[i])
	
	# stabilize the log transformation
	# max = 5; exp(5) ~= 148, way more than ever observed
	# min = -4; exp(-4) ~= 0.02, <<1 expected, way less than ever observed
	lambda2[i] <- max(-4, min(5, lambda1[i]))
	lambda.rich[i] <- exp(lambda2[i])
}#i
# 
}#model
", fill=TRUE)# closes cat
sink() # for group C


## group D:
## level of effort + habitat interaction with alpha and beta
# y ~ Poisson(lambda)
# log(lambda) = (A[xfac]*x1)/(B[xfac]+x1)
sink(paste(model.dir, "model_D.txt", sep="/"))
cat("
model{

# priors
## MM terms
for(i in 1:nfac){
    A[i] ~ dunif(0, 5)
    B[i] ~ dunif(0, 5)
}#i

# likelihood
for(i in 1:N){
    # Poisson draw for response variable
	y[i] ~ dpois(lambda.rich[i])
	
	# expected value
	lambda1[i] <- (A[xfac[i]] * x1[i])/(B[xfac[i]] + x1[i])
	
	# stabilize the log transformation
	# max = 5; exp(5) ~= 148, way more than ever observed
	# min = -4; exp(-4) ~= 0.02, <<1 expected, way less than ever observed
	lambda2[i] <- max(-4, min(5, lambda1[i]))
	lambda.rich[i] <- exp(lambda2[i])
}#i
# 
}#model
", fill=TRUE)# closes cat
sink() # for model D

## group E:
## level of effort + random effect of site on alpha
sink(paste(model.dir, "model_E.txt", sep="/"))
cat("
model{

# priors
## MM terms
mu.A ~ dunif(0, 5)
sigma.A ~ dunif(0.1, 10)
tau.A <- 1/pow(sigma.A, 2)
for(i in 1:nfac){A[i] ~ dnorm(mu.A, tau.A)}
B ~ dunif(0, 5)

# likelihood
for(i in 1:N){
    # Poisson draw for response variable
	y[i] ~ dpois(lambda.rich[i])
	
	# expected value
	lambda1[i] <- (A[xfac[i]] * x1[i])/(B + x1[i])
	
	# stabilize the log transformation
	# max = 5; exp(5) ~= 148, way more than ever observed
	# min = -4; exp(-4) ~= 0.02, <<1 expected, way less than ever observed
	lambda2[i] <- max(-4, min(5, lambda1[i]))
	lambda.rich[i] <- exp(lambda2[i])
}#i
# 
}#model
", fill=TRUE)# closes cat
sink() # for model E

## group F:
## level of effort + random effect of site on beta
sink(paste(model.dir, "model_F.txt", sep="/"))
cat("
model{

# priors
## MM terms
A ~ dunif(0, 5)

mu.B ~ dunif(0, 5)
sigma.B ~ dunif(0.1, 10)
tau.B <- 1/pow(sigma.B, 2)
for(i in 1:nfac){B[i] ~ dnorm(mu.B, tau.B)}

# likelihood
for(i in 1:N){
    # Poisson draw for response variable
	y[i] ~ dpois(lambda.rich[i])
	
	# expected value
	lambda1[i] <- (A * x1[i])/(B[xfac[i]] + x1[i])
	
	# stabilize the log transformation
	# max = 5; exp(5) ~= 148, way more than ever observed
	# min = -4; exp(-4) ~= 0.02, <<1 expected, way less than ever observed
	lambda2[i] <- max(-4, min(5, lambda1[i]))
	lambda.rich[i] <- exp(lambda2[i])
}#i
# 
}#model
", fill=TRUE)# closes cat
sink() # for model F

## group G:
## level of effort + random effect of site on alpha and beta
sink(paste(model.dir, "model_G.txt", sep="/"))
cat("
model{

# priors
## MM terms
mu.A ~ dunif(0, 5)
sigma.A ~ dunif(0.1, 10)
tau.A <- 1/pow(sigma.A, 2)
for(i in 1:nfac){A[i] ~ dnorm(mu.A, tau.A)}

mu.B ~ dunif(0, 5)
sigma.B ~ dunif(0.1, 10)
tau.B <- 1/pow(sigma.B, 2)
for(i in 1:nfac){B[i] ~ dnorm(mu.B, tau.B)}


# likelihood
for(i in 1:N){
    # Poisson draw for response variable
	y[i] ~ dpois(lambda.rich[i])
	
	# expected value
	lambda1[i] <- (A[xfac[i]] * x1[i])/(B[xfac[i]] + x1[i])
	
	# stabilize the log transformation
	# max = 5; exp(5) ~= 148, way more than ever observed
	# min = -4; exp(-4) ~= 0.02, <<1 expected, way less than ever observed
	lambda2[i] <- max(-4, min(5, lambda1[i]))
	lambda.rich[i] <- exp(lambda2[i])
}#i
# 
}#model
", fill=TRUE)# closes cat
sink() # for model G

#### Negative binomial models

## group H:
## level of effort only, no covariates, no random effect of site
# y ~ Negative binomial(p, r)
# p = r / (mu + r)
# log(mu) = MM(x1)
sink(paste(model.dir, "model_H.txt", sep="/"))
cat("
model{

# priors
## MM terms
A ~ dunif(0, 5)
B ~ dunif(0, 5)
# size parameter for negative binomial
# smaller r = more overdispersion relative to Poisson
r ~ dunif(0, 50)

# likelihood
for(i in 1:N){
    # negative binomial draw for response variable
	y[i] ~ dnegbin(p[i], r)
	
	# expected value
	# max = 5; exp(5) ~= 148, way more than ever observed
	# min = -4; exp(-4) ~= 0.02, <<1 expected, way less than ever observed
	mu1[i] <- (A * x1[i])/(B + x1[i])
	mu2[i] <- max(-4, min(5, mu1[i]))
	mu3[i] <- exp(mu2[i])	
	
    # transform mu to p parameter of negative binomial distribution
	# set minimum value 0.001, b/c p cannot be 0 per JAGS manual (p 51)
    p[i] <- max(0.001, r/(r + mu3[i]))
}#i
# 
}#model
", fill=TRUE)# closes cat
sink() # for model H

## group I:
## level of effort + habitat interaction with alpha
# y ~ Negative binomial(p, r)
# p = r / (mu + r)
# log(mu) = (A[xfac]*x1)/(B+x1)
sink(paste(model.dir, "model_I.txt", sep="/"))
cat("
model{

# priors
## MM terms
for(i in 1:nfac){
    A[i] ~ dunif(0, 5)
}#i

B ~ dunif(0, 5)
# size parameter for negative binomial
# smaller r = more overdispersion relative to Poisson
r ~ dunif(0, 50)

# likelihood
for(i in 1:N){
    # negative binomial draw for response variable
	y[i] ~ dnegbin(p[i], r)
	
	# expected value
	# max = 5; exp(5) ~= 148, way more than ever observed
	# min = -4; exp(-4) ~= 0.02, <<1 expected, way less than ever observed
	mu1[i] <- (A[xfac[i]] * x1[i])/(B + x1[i])
	mu2[i] <- max(-4, min(5, mu1[i]))
	mu3[i] <- exp(mu2[i])
	
    # transform mu to p parameter of negative binomial distribution
	# set minimum value 0.001, b/c p cannot be 0 per JAGS manual (p 51)
    p[i] <- max(0.001, r/(r + mu3[i]))
}#i
# 
}#model
", fill=TRUE)# closes cat
sink() # for model I

## group J:
## level of effort + habitat interaction with beta
# y ~ Negative binomial(p, r)
# p = r / (mu + r)
# log(mu) = (A*x1)/(B[xfac]+x1)
sink(paste(model.dir, "model_J.txt", sep="/"))
cat("
model{

# priors
## MM terms
A ~ dunif(0, 5)
for(i in 1:nfac){
    B[i] ~ dunif(0, 5)
}#i
# size parameter for negative binomial
# smaller r = more overdispersion relative to Poisson
r ~ dunif(0, 50)

# likelihood
for(i in 1:N){
    # negative binomial draw for response variable
	y[i] ~ dnegbin(p[i], r)
	
	# expected value
	# max = 5; exp(5) ~= 148, way more than ever observed
	# min = -4; exp(-4) ~= 0.02, <<1 expected, way less than ever observed
	mu1[i] <- (A * x1[i])/(B[xfac[i]] + x1[i])
	mu2[i] <- max(-4, min(5, mu1[i]))
	mu3[i] <- exp(mu2[i])	
	
    # transform mu to p parameter of negative binomial distribution
	# set minimum value 0.001, b/c p cannot be 0 per JAGS manual (p 51)
    p[i] <- max(0.001, r/(r + mu3[i]))
}#i
# 
}#model
", fill=TRUE)# closes cat
sink() # for group J


## group K:
## level of effort + habitat interaction with alpha and beta
# y ~ Negative binomial(p, r)
# p = r / (mu + r)
# log(mu) = (A[xfac]*x1)/(B[xfac]+x1)
sink(paste(model.dir, "model_K.txt", sep="/"))
cat("
model{

# priors
## MM terms
for(i in 1:nfac){
    A[i] ~ dunif(0, 5)
    B[i] ~ dunif(0, 5)
}#i
# size parameter for negative binomial
# smaller r = more overdispersion relative to Poisson
r ~ dunif(0, 50)


# likelihood
for(i in 1:N){
    # negative binomial draw for response variable
	y[i] ~ dnegbin(p[i], r)
	
	# expected value
	# max = 5; exp(5) ~= 148, way more than ever observed
	# min = -4; exp(-4) ~= 0.02, <<1 expected, way less than ever observed
	mu1[i] <- (A[xfac[i]] * x1[i])/(B[xfac[i]] + x1[i])
	mu2[i] <- max(-4, min(5, mu1[i]))
	mu3[i] <- exp(mu2[i])	
	
    # transform mu to p parameter of negative binomial distribution
	# set minimum value 0.001, b/c p cannot be 0 per JAGS manual (p 51)
    p[i] <- max(0.001, r/(r + mu3[i]))
}#i
# 
}#model
", fill=TRUE)# closes cat
sink() # for model K

## group L:
## level of effort + random effect of site on alpha
# y ~ Negative binomial(p, r)
# p = r / (mu + r)
# log(mu) = (A[xfac]*x1)/(B+x1)
sink(paste(model.dir, "model_L.txt", sep="/"))
cat("
model{

# priors
## MM terms
mu.A ~ dunif(0, 5)
sigma.A ~ dunif(0.1, 10)
tau.alpha <- 1/pow(sigma.A, 2)
for(i in 1:nfac){A[i] ~ dnorm(mu.A, tau.alpha)}
B ~ dunif(0, 5)
# size parameter for negative binomial
# smaller r = more overdispersion relative to Poisson
r ~ dunif(0, 50)

# likelihood
for(i in 1:N){
    # negative binomial draw for response variable
	y[i] ~ dnegbin(p[i], r)
	
	# expected value
	# max = 5; exp(5) ~= 148, way more than ever observed
	# min = -4; exp(-4) ~= 0.02, <<1 expected, way less than ever observed
	mu1[i] <- (A[xfac[i]] * x1[i])/(B + x1[i])
	mu2[i] <- max(-4, min(5, mu1[i]))
	mu3[i] <- exp(mu2[i])
	
    # transform mu to p parameter of negative binomial distribution
	# set minimum value 0.001, b/c p cannot be 0 per JAGS manual (p 51)
    p[i] <- max(0.001, r/(r + mu3[i]))
}#i
# 
}#model
", fill=TRUE)# closes cat
sink() # for model L

## group M:
## level of effort + random effect of site on beta
# y ~ Negative binomial(p, r)
# p = r / (mu + r)
# log(mu) = (A*x1)/(B[xfac]+x1)
sink(paste(model.dir, "model_M.txt", sep="/"))
cat("
model{

# priors
## MM terms
A ~ dunif(0, 5)

mu.B ~ dunif(0, 5)
sigma.B ~ dunif(0.1, 10)
tau.beta <- 1/pow(sigma.B, 2)
for(i in 1:nfac){B[i] ~ dnorm(mu.B, tau.beta)}

# size parameter for negative binomial
# smaller r = more overdispersion relative to Poisson
r ~ dunif(0, 50)


# likelihood
for(i in 1:N){
    # negative binomial draw for response variable
	y[i] ~ dnegbin(p[i], r)
	
	# expected value
	# max = 5; exp(5) ~= 148, way more than ever observed
	# min = -4; exp(-4) ~= 0.02, <<1 expected, way less than ever observed
	mu1[i] <- (A * x1[i])/(B[xfac[i]] + x1[i])
	mu2[i] <- max(-4, min(5, mu1[i]))
	mu3[i] <- exp(mu2[i])
	
    # transform mu to p parameter of negative binomial distribution
	# set minimum value 0.001, b/c p cannot be 0 per JAGS manual (p 51)
    p[i] <- max(0.001, r/(r + mu3[i]))
}#i
# 
}#model
", fill=TRUE)# closes cat
sink() # for model M

## group N:
## level of effort + random effect of site on alpha and beta
# y ~ Negative binomial(p, r)
# p = r / (mu + r)
# log(mu) = (A[xfac]*x1)/(B[xfac]+x1)
sink(paste(model.dir, "model_N.txt", sep="/"))
cat("
model{

# priors
## MM terms
mu.A ~ dunif(0, 5)
sigma.A ~ dunif(0.1, 10)
tau.alpha <- 1/pow(sigma.A, 2)
for(i in 1:nfac){A[i] ~ dnorm(mu.A, tau.alpha)}

mu.B ~ dunif(0, 5)
sigma.B ~ dunif(0.1, 10)
tau.beta <- 1/pow(sigma.B, 2)
for(i in 1:nfac){B[i] ~ dnorm(mu.B, tau.beta)}

# size parameter for negative binomial
# smaller r = more overdispersion relative to Poisson
r ~ dunif(0, 50)


# likelihood
for(i in 1:N){
    # negative binomial draw for response variable
	y[i] ~ dnegbin(p[i], r)
	
	# expected value
	# max = 5; exp(5) ~= 148, way more than ever observed
	# min = -4; exp(-4) ~= 0.02, <<1 expected, way less than ever observed
	mu1[i] <- (A[xfac[i]] * x1[i])/(B[xfac[i]] + x1[i])
	mu2[i] <- max(-4, min(5, mu1[i]))
	mu3[i] <- exp(mu2[i])
	
    # transform mu to p parameter of negative binomial distribution
	# set minimum value 0.001, b/c p cannot be 0 per JAGS manual (p 51)
    p[i] <- max(0.001, r/(r + mu3[i]))
}#i
# 
}#model
", fill=TRUE)# closes cat
sink() # for model N

# end script!