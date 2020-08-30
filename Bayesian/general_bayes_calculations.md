# Bayesian statistics.
### Mathematics (derivatives, differentiation, logarithms)
### Optimization and MLE: maximum likelihood estimate
### Posterior, Prio, Likelihoods; Probability densities and distributions.


###### Below should be a mathematical representation of how MLE is determined

$L(\phi|y_1,y_2,...,y_n)=\frac{1}{2\pi}^{\frac{n}{2}}*\phi^\frac{n}{2}*e^{\frac{-\phi}{2}\sum_{i=1}^{n} (y_i-100)^2}$


$L(\phi|y_1,y_2,...,y_n)=\log(\frac{1}{2\pi}^{\frac{n}{2}}*\phi^\frac{n}{2}*e^{\frac{-\phi}{2}\sum_{i=1}^{n} (y_i-100)^2})$
Differentiation easier with log form

$L(\phi|y_1,y_2,...,y_n)=\log(\frac{1}{2\pi}^{\frac{n}{2}}) + \log(\phi^\frac{n}{2}) + \log(e^{\frac{-\phi}{2}\sum_{i=1}^{n} (y_i-100)^2})$

Use log product rules

$L(\phi|y_1,y_2,...,y_n)={\frac{n}{2}}\log(\frac{1}{2\pi}) + \frac{n}{2}\log(\phi) + \frac{-\phi}{2}\sum_{i=1}^{n} (y_i-100)^2 \log(e)$

##### Log rules

$L`(\phi|y_1,y_2,...,y_n)=\frac{n}{2}*\frac{1}{\phi} - \frac{1}{2}\sum_{i=1}^{n} (y_i-100)^2$

###### Differentiation rules

$\frac{n}{2}*\frac{1}{\phi} - \frac{1}{2}\sum_{i=1}^{n} (y_i-100)^2=0$
Find maximum -> f`(x) = 0

$\frac{n}{2}*\frac{1}{\phi} = \frac{1}{2}\sum_{i=1}^{n} (y_i-100)^2$

##### Moving variables around

$ \phi = n / \sum_{i=1}^{n} (y_i-100)^2 $

##### Expression after manipulating by multiplying by two.

$ \frac{1}{\phi} = \sum_{i=1}^{n} (y_i-100)^2  / n $


##### Calculating MLE given the value of parameters and sample.

par1 <- 493
n <- 15
phimle <- n/par1
phimle


##### Optimization using MLE
###### Calculating standard derivative based on given information

sd = sqrt(par1/n)
m = 100
set.seed(2)
x <- rnorm(n,m,sd)

funct <- function(phi){
  par2 <- 100
  prod((1/(2*pi))^(n/2)*phi^(n/2)*exp((-phi/2)*sum(x-par2)^2))
}
optimize(funct,interval=c(0,1),maximum = TRUE)$maximum

funct1 <- function(phi){
  (n/2)*log(pi) + n/2*log(phi) - (phi/2)*493 # loge is 1
  }
optimize(funct1,interval=c(0,1),maximum = TRUE)$maximum # The same as in part a.

###### Visualizing Bayes formula and applying it to the case above.
###### Picking prior based on given data. Multiniomal

$Posterior\propto Likelihood  * Prior$


$L(\phi|y_1,y_2,...,y_n)=\frac{1}{2\pi}^{\frac{n}{2}}*\phi^\frac{n}{2}*exp(\frac{-\phi}{2}\sum_{i=1}^{n} (y_i-100)^2)$

$P(\phi)=\frac{\beta}{\Gamma(\alpha)}*\phi^{\alpha-1}*e^{-\beta\phi}$

$P(\phi|y)\propto\frac{1}{2\pi}^{\frac{n}{2}}*\phi^\frac{n}{2}*exp(\frac{-\phi}{2}\sum_{i=1}^{n} (y_i-100)^2) * \frac{\beta}{\Gamma(\alpha)}*\phi^{\alpha-1}*exp({-\beta\phi)}$

$P(\phi|y)\propto\phi^\frac{n}{2}*exp(\frac{-\phi}{2}\sum_{i=1}^{n} (y_i-100)^2)*\phi^{\alpha-1}*exp({-\beta\phi)}$ # Taking the constants away

$P(\phi|y) \propto \phi^{\frac{n}{2}+\alpha-1}*exp(\frac{-\phi}{2}\sum_{i=1}^{n} (y_i-100)^2-\beta\phi)$

##### Same base variables applying basic algebra rules, where alpha_s=a+n/2; beta_s=beta+1/2*sigma(yi-100)^2

alpha <- 100
####### e(x) = a/b
beta <- 1
b <- phimle


##### Setting a prior function
grid=seq(50,180,1)  
priorpmf.grid = dgamma(grid, shape=alpha, rate = beta)
plot(grid, priorpmf.grid, type="l",
     main="Prior setting if mu",xlab='mu',ylab='density')

##### This is prior visualization for mu, which is more intuitive, the values around 100 are more probable and values around the edges are less probable.

alphaa <- 3
betaa <- 3
grid1=seq(0,2,0.1)
priorpmf.grid1= dgamma(grid1, shape=alphaa, rate=betaa)

##### Picking reasonable alpha and beta based on chosen prior and statistical conditions.
plot(grid1, priorpmf.grid1, type="l", main="Prior for phi",xlab='phi',ylab='density')


##### Deriving Joint Posterior function
grid1=seq(0,2,0.1)
alpha <-3
beta <-3
alphas <- alpha+n/2 # astar
betas <- beta + 0.5*493 # bstar
priorpmf.grid1= dgamma(grid1, shape=alpha, rate =beta)
posteriorpmf.grid1 <- dgamma(grid1,shape = alphas, rate = betas)

plot(grid1, priorpmf.grid1, type="l",main="Prior for phi",xlab='phi',ylab='density',ylim=c(0,1))
lines(grid1,posteriorpmf.grid1,col='blue')

### Gibbs Sampler and Data simulation

n    <- 15
ybar <- 106.9
sigmay <- 114293

##### sample from the joint posterior
mu     <- numeric()
ti    <- numeric()
mu[1] <- rnorm(1, mean = 100, sd = 15^2)  # initialisation

for(i in 1:10000){ # M=10'000
    ti[i] = rgamma(n=1,shape = 3 + n/2, rate = 3 + (sigmay + 2*n*mu[i]*ybar + n*(mu[i]) ^2)/2)
    mu[i + 1] = rnorm(n=1,mean=(ti[i]*n*ybar + 100/(n^2))*(n*ti[i] + 1/(15^2))^(-1), sd = sqrt((n*ti[i]+  1/(15^2))^-1))
}


layout(matrix(c(1:6),2,3))
plot(mu,type='l')
abline(h=mean(mu),col='red')
plot(ti,type='l')
abline(h=mean(ti),col='red')
hist(mu)
hist(ti)

###### Checking for autocorrelation; the relation between data simulated first and data that was simulated last. If correlation is high, simulated data isn't good for a bayesian statistics analysis.
acf(mu)
acf(ti)
