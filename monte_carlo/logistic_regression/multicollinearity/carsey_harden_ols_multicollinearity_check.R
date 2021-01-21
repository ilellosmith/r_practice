library(mvtnorm)
set.seed(121402)
# number of reps 
reps <- 1000
# matrix to store estimates
par.est.mc <- matrix(NA, nrow = reps, ncol = 4)

# set true values
b0 <- 0.2 
b1 <- 0.5
b2 <- 0.75

# set sample size
n <- 1000

# Define levels of multicollinearity
mc.level <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99)
# Create matrix to store sd of estimates 
sd.betas <- matrix(NA, nrow = length(mc.level), ncol = 2)

# iterate over levels of multicollinearity
for (j in 1:length(mc.level)){
  X.corr <- matrix(c(1, mc.level[j], mc.level[j], 1), nrow = 2, ncol =2)
  X <- rmvnorm(n, mean = c(0,0), sigma = X.corr)
  X1 <- X[ ,1]
  X2 <- X[ ,2]
  
  for (i in 1:reps) {
    Y <- b0 + b1*X1 + b2*X2 + rnorm(n, 0,1)
    model <- lm(Y ~ X1 + X2)
    vcv <- vcov(model) 
    par.est.mc[i,1] <- model$coef[2]
    par.est.mc[i,2] <- model$coef[3]
    par.est.mc[i,3] <- sqrt(diag(vcv))[2]
    par.est.mc[i,4] <- sqrt(diag(vcv))[3]
  }
  sd.betas[j, ] <-c(sd(par.est.mc[,1]-b1), sd(par.est.mc[,1]))
  cat('Just completed correlation =', mc.level[j],
      '(', j,'of', length(mc.level), ')', '\n')
}

plot(mc.level, sd.betas[,1])
