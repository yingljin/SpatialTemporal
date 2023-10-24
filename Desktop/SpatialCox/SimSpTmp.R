# This package messes around with the spacetime R package
# model after the GSRE paper

library(tidyverse)
library(STRbook)


#### Load data ####



# https://cran.r-project.org/web/packages/stppSim/vignettes/stppSim-vignette.html

# First only think about spatial correlation
# Following the GSRE model in the Millo paper

N <- 100 # sample size
J <- 50 # spatial index

lambda <- 0.6 # spatial parameter in fixed effects
rho1 <- 0.6 # spatial parameter in individual-level random effect
rho2 <- 0.6 # spatial parameter in error term
# variance of all noises set to 1


df <- data.frame(id = rep(1:N, each = J),
                 t = rep(1:J, N))
df$id <- as.factor(df$id)

# AR1 spatial weight matrix
cor_mat <- Initialize(corAR1(0.2, form =  ~ t | id), data = df)
cor_mat <- corMatrix(cor_mat)
spwt <- cor_mat[[1]]
diag(spwt) <- 0


# generate the spatially correlated random error
epsilon <- solve(diag(1, N*J, N*J)- rho2 * (diag(1, N, N) %x% spwt)) %*% rnorm(N*J)

# generate spatially correlated individual effects
mu <- solve(diag(1, J, J) - rho1 * spwt) %*% rnorm(J)

# composite error
u <- (rep(1, N) %x% diag(1, J, J))%*%mu + epsilon

# final outcome
Y <- solve(diag(1, N*J, N*J)- lambda * (diag(1, N, N) %x% spwt)) %*% u
  