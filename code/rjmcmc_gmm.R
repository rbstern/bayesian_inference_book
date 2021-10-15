library(tidyverse)

# D ~ Geometrica(0.5) (truncada em K)
# mu_1, ..., mu_D i.i.d. ~ N(0, 10)
# tau^2_1, ..., tau^2_D i.i.d. ~ G(1, 1)
# p ~ Dirichlet(1, ..., 1)
# z_1, ..., z_n i.i.d. P(Z=i) = p[i]
# x_i|z_i ~ N(mu[z_i], 1/tau^2[z_i])

# Geração dos dados segundo um GMM
mu <- c(10, 30, 20)
sigma <- c(3, 1, 2)
p <- c(0.3, 0.2, 0.5)
n <- 1000
z <- sample(1:3, n, replace = TRUE, prob = p) 
x <- rnorm(n, mu[z], sigma[z])

# M ~ Geom(0.5) (truncada em K)
# phi_j|M = j ~ i.i.d. N(0, 0.3^2)
# Y|phi_j, M = j ~ AR(j, phi_j)

# M = i
# (mu)
# M = j

# f(m, phi|y) \propto
# f(m)f(phi|m)f(y|m,theta)

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

# param tem dimensão K+1
# param[1]: é o modelo: 0, 1, ..., K
# param[2:(K+1)]: ou phi ou u
ldtgt <- function(param)
{
  d = param[1]
  phi = param[2:(param[1]+1)]
  u = param[(param[1]+2):(K+1)]
  if(param[1] == 0) phi = numeric()
  if(param[1] == K) u = numeric()
  aux = log(dgeom(param[1], 0.5)) +
    sum(log(dnorm(phi, 0, 0.3))) +
    sum(log(dnorm(u, 0, 0.3))) +
    sum(log(dnorm(y[1:d])))
  for(ii in (d+1):n)
  {
    mu = sum(phi*y[(ii-1):(ii-d)])
    aux = aux + log(dnorm(y[ii], mu))
  }
  aux
}

p = 0.2
# (1)     (2)     (3)   ...      (K)
#  |--->   |       |              |
#      <---|--->   |              |
#              <---|--->          |
#               ...               |
#                             <---| 
rprop <- function(param)
{
  if(runif(1) <= p)
  {
    case_when(
      param[1] == 0 ~ c(1, param[2:(K+1)]),
      param[1] == K ~ c(K-1, param[2:(K+1)]),
      TRUE ~ c(param[1] + sample(c(-1, 1), 1), 
               param[2:(K+1)])
    )
  }
  else
  {
    prop = param
    dim = sample(2:(K+1), 1)
    prop[dim] = prop[dim] + rnorm(1, 0, 0.3)
    prop
  }
}  

ldprop <- function(prop, param)
{
  if(param[1] != prop[1])
  {
    aux = log(p)
    if(param[1] != 1 && param[1] != K) aux = aux + log(0.5)
    aux
  }
  aux = log(1-p) + log(1/K)
  for(ii in 2:(K+1))
  {
    if(prop[ii] != param[ii])
    {
      aux = aux + log(dnorm(prop[ii]-param[ii], 0, 0.3))
    }
  }
  aux
}

B = 10^4
start = rep(0, K+1)
aux = metropolis(rprop, ldprop, ldtgt, B, start)
aux = aux %>% 
  unlist() %>% 
  matrix(nrow = B, ncol = K+1, byrow = TRUE)

dt = tibble(M = aux[,1],
            phi = aux[,2:(K+1)])

dt$M %>% 
  table()

phi = dt %>% 
  filter(M == 2) %>% 
  select(phi)

phi %>% colMeans()

phi[[1]] %>% boxplot()