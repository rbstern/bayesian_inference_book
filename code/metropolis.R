library(tidyverse)

metropolis <- function(rprop, ldprop, ldtgt, B = 10^4, start = 0)
{
  chain <- as.list(rep(NA, B))
  chain[[1]] <- start
  for(ii in 2:B)
  {
    prop <- rprop(chain[[ii-1]])
    lratio <- ldtgt(prop)-ldtgt(chain[[ii-1]])+
      ldprop(prop,chain[[ii-1]])-
      ldprop(chain[[ii-1]],prop)
    if(log(runif(1)) <= lratio) chain[[ii]] <- prop
    else chain[[ii]] <- chain[[ii-1]]
  }
  return(chain)
}

# Theta ~ N(0, 1)
# Z|Theta ~ N(theta, 1)
# Theta|Z = 10

z <- 10
ldtgt <- function(y) log(dnorm(y, mean = 0, sd = 1)) + 
  log(dnorm(z, mean = y, sd = 1))
rprop <- function(x) rnorm(1, mean = x, sd = 1)
ldprop <- function(x, y) log(dnorm(y, mean = x, sd = 1))

chain <- metropolis(rprop, ldprop, ldtgt) %>% unlist()

# Estimador para theta
mean(chain)

# Intervalo de credibilidade
quantile(chain, probs = c(0.025, 0.975))

# Testar a hipótese de que Theta > 7
mean(chain > 7)

