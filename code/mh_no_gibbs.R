#a ~ Exp(1), b ~ Exp(1), a || b
#X|a,b ~ c.i.i.d. Gamma(a,b)
#log(f(a|X,b)) ~ log(f(a)) + log(f(X|a,b))
#log(f(b|X,a)) ~ log(f(b)) + log(f(X|a,b))

a_v <- 10
b_v <- 20
X <- rgamma(100, a_v, rate = b_v)

ldconda <- function(a, X, b)
{
  n = length(a)
  ld <- rep(NA, n)
  for(ii in 1:n) 
  {
    ld[ii] <- log(dexp(a[ii])) + 
      sum(log(dgamma(X, a[ii], rate = b)))
  }
  ld
}
  
#teste <- function(a) ldconda(a, X, b_v)
#curve(teste, from = 0.01, to = 30)
  
ldcondb <- function(b, X, a)
{
  n = length(b)
  ld <- rep(NA, n)
  for(ii in 1:n) 
  {
    ld[ii] <- log(dexp(b[ii])) + 
      sum(log(dgamma(X, a, rate = b[ii])))
  }
  ld
}

#teste <- function(b) ldcondb(b, X, a_v)
#curve(teste, from = 0.01, to = 100)

metropolis <- function(rprop, ldprop, ldtgt, B = 10^3, start = 0)
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

prop1 <- function(X, b)
{
  rprop <- function(y) rexp(1, rate = 1/y)
  ldprop <- function(x, y) log(dexp(x, rate = 1/y))
  ldtgt <- function(y) ldconda(y, X, b)
  chain = metropolis(rprop, ldprop, ldtgt, B = 10^3, start = 1)
  chain[[10^3]]
}
  
prop2 <- function(X, a)
{
  rprop <- function(b) rexp(1, rate = 1/b)
  ldprop <- function(y, b) log(dexp(y, rate = 1/b))
  ldtgt <- function(b) ldcondb(b, X, a)
  chain = metropolis(rprop, ldprop, ldtgt, B = 10^3, start = 1)
  chain[[10^3]]
}

K <- 10^2
A <- rep(NA, K)
B <- rep(NA, K)
A[1] = 10
B[1] = 20
for(ii in 2:K)
{
  d = sample(1:2, 1)
  if(d == 1)
  {
    B[ii] = B[ii-1]
    A[ii] = prop1(X, B[ii-1])
  }
  if(d == 2)
  {
    A[ii] = A[ii-1]
    B[ii] = prop2(X, A[ii-1])
  }
}

plot(density(A))
plot(density(B))

# Como A e B são muito correlacionados,
# O Gibbs não funciona bem
