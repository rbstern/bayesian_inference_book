library(rstan)
library(tidyverse)

# Exemplo com dados simulados
# Dados
mu = 5
sigma = 2
N = 20
Y = rnorm(N, mu, sigma)

# Rodar o Stan
aux = stan(
  "./code/stan_normal_normal.stan",
  data = list(N = N, Y = Y)
)

# Exemplo de Inferencia

plot(aux)
mu = extract(aux)$mu
tau = extract(aux)$tau

# Estimacao
mean(mu)
mean(tau)

# Intervalo de credibilidade
quantile(mu, probs = c(0.025, 0.975))

# Teste de hipotese
mean(mu >= 4.5)
