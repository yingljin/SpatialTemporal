
# Simulate spatial-temporal data for multiple subjects

#### set-up ####

set.seed(1026)

library(mgcv)
library(nlme)
library(tidyverse)
library(plotly)
library(mvtnorm)
library(ggpubr)
library(geoR)
library(MBESS)

#### simulation set up ####
N <- 10 # sample size 
P <- 3 # number of fixed covariates
K <- 2 # number of random covariates

#### Measurement grid ####
# spatial
# 2D space, 21 points along each axis, 441 spatial points in total
s1 <- s2 <- seq(-1, 1, by = 0.1)
coords <- expand.grid(s1, s2) 
nS <- nrow(coords)

# temporal grid
# five measurement time
t <- seq(0.2, 1, by = 0.2)
nT <- length(t)

df <- expand.grid(id = 1:N, s1=s1, s2=s2, t=t)


#### Covariance and distance for spatial distribution ####

# assign an index for each spatial point
coords$pid <- 1:nS
colnames(coords) <- c("s1", "s2", "pid")
df <- df %>% left_join(coords, by = c("s1", "s2"))

# calculate pairwise spatial Euclidiean distance matrix
df_dist <- dist(coords %>% select(s1, s2), diag = T)
df_dist <- as.matrix(df_dist)
dim(df_dist)

# calculate Matern correlation matrix
sp_cormat1 <- matern(df_dist, phi = 0.25, kappa = 0.25)
sp_cormat2 <- matern(df_dist, phi = 0.5, kappa = 0.5)

# Convert to covariance matrix
sp_covmat1 <- cor2cov(sp_cormat1, sd = rep(1, nS))
sp_covmat2 <- cor2cov(sp_cormat2, sd = rep(1, nS))


#### Generate Coefficients ####
# fixed
alpha <- c(0.2, -0.2, 0.1)
beta <- data.frame(t) %>% 
  mutate(beta1 = alpha[1]/t,
         beta2 = alpha[2]/t,
         beta3 = alpha[3]/t)

# random
xi <- rmvnorm(N, mean = rep(0, K), sigma = diag(1, K, K))

#### Generate Covariates ####

# fixed effects: Y ~ X1(S)+t+X2
X1_mat <- rmvnorm(N, mean = rep(0, nS), sigma = sp_covmat1)
X1_mat <- data.frame(id = rep(1:N, each = nS),
                     X1 = as.vector(t(X1_mat)), 
                     pid = rep(1:nS, N)) 
df <- df %>% left_join(X1_mat)

X2_mat <- rnorm(N)
X2_mat <- data.frame(id = 1:N, X2 = X2_mat)
df <- df %>% left_join(X2_mat)


# random effects


i <- 1
for(i in 1:N){
  
  # generate fixed covariates
  ## spatial
  X1_i <- rmvnorm(1, mean = rep(0, nS), sigma = sp_covmat1)
  df[df$id==i, "X1"] <- rep(X1_i, nT)
  # temporal
  X2_i <- 10*rnorm(1)*t
  df[df$id==i, "X2"] <- rep(X2_i, each = nS)
  df_sp_i <- data.frame(coords, X1=X1[1, ])
  # fixed
  X3_i <- rnorm(1)
  df[df$id==i, "X3"] <- X3_i
  
  #generate random covariates
  phi_i <- rmvnorm(1, mean = rep(0, nS), sigma = sp_covmat2)
  df[df$id==i, "phi"] <- rep(phi_i, nT)
}