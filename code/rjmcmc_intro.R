library(tidyverse)

# Geração dos dados
n <- 3
x <- rnorm(n)
y <- rnorm(n, 5 + 10*x)

# P(M1) = P(M2) = 0.5
# mu ~ N(0,100)
# Y|mu, M1 ~ N(mu, 1)
# mu* ~ N(0, 100)
# beta ~ N(0, 100)
# Y|mu*, beta, M2 ~ N(mu + beta*X, 1)

# mu,  u
#  |   |
# mu*, beta

# Uma possibilidade:
# h(mu, u) = (mu*, u)
# isto é, mu* = u, beta = u

# log(f(m, theta|y,x)) \propto
# log(f(m)) + log(f(theta|m)) + log(f(y|m,theta,x))

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
    chain[[ii]] <- chain[[ii-1]]
    if(log(runif(1)) <= lratio) chain[[ii]] <- prop
  }
  return(chain)
}

# param tem dimensão 3
# param[1]: é o modelo: (0 ou 1)
# param[2]: mu ou mu*
# param[3]: u ou beta
ldtgt <- function(param)
{
  case_when(
    param[1] == 0 ~
      log(0.5) + log(dnorm(param[2], 0, 10)) + 
      log(dnorm(param[3], 0, 10)) +
      sum(log(dnorm(y, param[2]))), 
    param[1] == 1 ~
      log(0.5) + log(dnorm(param[2], 0, 10)) + 
      log(dnorm(param[3], 0, 10)) + 
      sum(log(dnorm(y, param[2] + param[3]*x)))
  )
}

p = 0.2
rprop <- function(param)
{
  if(runif(1) <= p)
    c(1-param[1], param[2], param[3])
  else
  {
    prop = param
    dim = sample(2:3, 1)
    prop[dim] = prop[dim] + rnorm(1)
    prop
  }
}  
  
ldprop <- function(prop, param)
{
  if(param[1] != prop[1])
    return(log(p))
  aux = log(1-p) + log(0.5)
  for(ii in 2:3)
  {
    if(prop[ii] != param[ii])
    {
      aux = aux + log(dnorm(prop[ii]-param[ii]))
    }
  }
  aux
}

B = 10^4
aux = metropolis(rprop, ldprop, ldtgt, B, c(0, 0, 0))
aux = aux %>% 
  unlist() %>% 
  matrix(nrow = B, ncol = 3, byrow = TRUE)

dt = tibble(M = aux[,1],
            mu = aux[,2],
            beta = aux[,3])

dt$M %>% mean()

dt %>% 
  filter(M == 1) %>% 
  select(mu) %>% 
  unlist() %>% 
  boxplot()

dt %>% 
  filter(M == 1) %>% 
  select(beta) %>% 
  unlist() %>% 
  boxplot()
