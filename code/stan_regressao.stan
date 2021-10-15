data {
  int<lower=0> N;   
  int<lower=0> K;   
  matrix[N, K] X;   
  vector[N] Y;      
}

parameters {
  real alpha;
  vector[K] beta;
  real<lower=0> tau;
}

model {
  tau ~ gamma(1, 1);
  beta ~ normal(0, 1);
  Y ~ normal(X * beta + alpha, 1/tau);
}
