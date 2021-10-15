library(tidyverse)
library(rstan)

# Exemplo de modelo de regressao simulado
beta = 3:5
tau = 49
K = length(beta)
N = 100
X = rnorm(N*D) %>% 
  matrix(nrow = N, ncol = K)
Y = X %*% beta + rnorm(N, 0, 1/tau)
Y = as.numeric(Y)

aux = stan("./code/stan_regressao.stan",
           data = list(N = N, K = K, X = X, Y = Y))

plot(aux)

tau = extract(aux, "tau")$tau
mean(tau)
quantile(tau, probs = c(0.025, 0.975))
