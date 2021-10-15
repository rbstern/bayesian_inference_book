# SIR - Sequential Importance Resampling
# rprop: função que gera o próximo estado da partícula
# peso: peso dado para cada partícula
# d: número de dimensões do vetor que desejamos simular
# B: número de partículas
SIR <- function(rprop, peso, d, B = 10^4)
{
  particles <- matrix(NA, nrow = B, ncol = d)
  for(jj in 1:d)
  {
    pesos <- rep(NA, B)
    for(ii in 1:B)
    {
      passado <- particles[ii, 1:(jj-1)]
      if(jj == 1) { passado <- c() }
      particles[ii,jj] <- rprop(passado)
      pesos[ii] <- peso(particles[ii,1:jj])
    }
    linhas = sample(1:B, B, replace = TRUE, prob = pesos)
    particles <- particles[linhas,]
  }
  particles
}

# Exemplo com um AR(1) com erro de medição com phi = 0.2
# T1 -->  T2 --> T3 --> ... --> Td
# |       |      |              |
# X1,1    X2,1   X3,1           Xd,1
# X1,2    X2,2   X3,2           Xd,2
d = 100
phi = 0.2
sigma_t = 1
sigma_x = 0.9
theta <- rep(NA, d)
theta[1] <- 0
for(ii in 2:d)
{
  theta[ii] = rnorm(1, 0.2*theta[ii-1], sigma_t)
}
x1 <- rnorm(d, theta, sigma_x)
x2 <- rnorm(d, theta, sigma_x)
plot(as.ts(theta), col = "red")
lines(as.ts(x1), col = "green")
lines(as.ts(x2), col = "blue")

# f(theta[1:n]|x[1:n]) ~ f(theta[1:(n-1)]|x[1:(n-1)])*
#                        f(theta[n]|theta[n-1])*f(x[n]|theta[n])

# f(theta[1:n]|x[1:n]) ~ prod(f(theta[i]|theta[i-1])*f(x[i]|theta[i]))


#Função para usar o SIR
# g(theta[i]|theta[i-1]) ~ N(0.2*theta[i-1], sigma_t)
rprop <- function(theta)
{
  d = length(theta)
  if(d == 0) return(rnorm(1, 0, sigma_t))
  rnorm(1, phi*theta[d], sigma_t)
}

# f(theta[1:n]|x[1:n])            prod(f(x[i]|theta[i]))
# ---------------------------- =
# prod(f(theta[i]|theta[i-1]))    

# w[i] = f(x[i]|theta[i])
# w[i] = f(x1[i]|theta[i])*f(x2[i]|theta[i])

peso <- function(theta)
{
  d = length(theta)
  aux1 = dnorm(x1[d], theta[d], sigma_x)
  aux2 = dnorm(x2[d], theta[d], sigma_x)
  aux1*aux2
}

particles <- sir(rprop, peso, d)

dados <- tibble(t = 1:d, 
                media = colMeans(particles),
                sd = sqrt(apply(particles, 2, var)))

dados %>% 
  ggplot(aes(x = t, y = media)) +
  geom_line() +
  geom_ribbon(aes(ymin = media-2*sd, 
                  ymax = media+2*sd), 
              alpha=0.1, 
              linetype="dashed",
              color="grey") +
  geom_line(aes(x = t, y = theta, color = "red"))

