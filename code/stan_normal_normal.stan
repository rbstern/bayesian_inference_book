// 1. Especificar dados
data {
  int<lower=0> N;
  vector[N] Y;
}

// 2. Especificar parametros
parameters {
  real mu;
  real<lower=0>tau;
}

// 3. Especificar o modelo
model {
  mu ~ normal(0, 10);
  tau ~ gamma(1, 1);
  Y ~ normal(mu, 1/tau);
}
