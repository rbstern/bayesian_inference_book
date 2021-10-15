BB <- 10^4
x1 = 10
x2 = 5
prop_lambda = function(mu1, mu2)
  rnorm(1, mean = 1/3*(mu1+mu2), sd = sqrt(1/3))
prop_mu1 = function(lambda)
  rnorm(1, mean = 0.5*(lambda+x1), sd = sqrt(1/2))
prop_mu2 = function(lambda)
  rnorm(1, mean = 0.5*(lambda+x2), sd = sqrt(1/2))

mu1 <- rep(NA, BB)
mu2 <- rep(NA, BB)
lambda <- rep(NA, BB)
mu1[1] = 0
mu2[1] = 0
lambda[1] = 0

for(ii in 2:BB)
{
  d <- round(runif(1, 0, 3))
  lambda[ii] <- ifelse(d == 0,
                       prop_lambda(mu1[ii-1], mu2[ii-1]),
                       lambda[ii-1])
  mu1[ii] <- ifelse(d == 1,
                    prop_mu1(lambda[ii-1]),
                    mu1[ii-1])
  mu2[ii] <- ifelse(d == 2,
                    prop_mu2(lambda[ii-1]),
                    mu2[ii-1])
}

plot(density(mu1))
plot(density(mu2))
plot(density(lambda))
