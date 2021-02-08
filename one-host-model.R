#Load package
library(deSolve)

#SEIR Model for One Vertebrate and Vector Model 
seir <- function(times, init, parameters) {
  
  Sh <- init[1] #susceptible hosts
  Eh <- init[2] #exposed hosts
  Ih <- init[3] #infected hosts
  Rh <- init[4] #recovered hosts
  Nh <- Sh + Eh + Ih + Rh #total population of hosts
  
  Sv <- init [5] #susceptible vector
  Ev <- init [6] #exposed vector
  Iv <- init [7] #infected vector
  Vv <- Sv + Ev + Iv #total popuation of vector
  #assumed that vectors remain infected for life
  
  with(as.list(c(parameters)), {
    
    #mu1 = birth rate of vertebrate
    #upsilon = rate of waning immunity
    #lambda1 = natural death rate of vertebrate 
    #sigma1 = rate of infected to infectious in vertebrate
    #lambda2 = death rate due to infection 
    #gamma = rate of recovery 
    #a1 = portion of tsetse bloodmeals/duration of feeding cycles in fly
    #b1 = probability of infected fly bite giving rise to 
    #     infection in species 1
    
    dSh <- mu1 * Nh + upsilon * Rh - a1*b1 * Sh * Iv/Nh - lambda1 * Sh
    dEh <- a1*b1 * Sh * Iv /Nh - sigma1 * Eh - lambda1 * Eh
    dIh <- sigma1 * Eh - lambda1 * Ih - lambda2 * Ih - gamma * Ih
    dRh <- gamma * Ih - lambda1 * Rh - upsilon * Rh
    
    #mu2 = birth rate of vector
    #lambda3 = natural death rate of vector
    #sigma2 = rate of infected to infectious in vector 
    #T= incubation period
    #exp = probability of survival
    #c = probability of infected bloodmeal giving rise to infection in fly
    #a1 = portion of tsetse bloodmeals/duration of feeding cycles in fly
    
    dSv <- mu2 * Vv - exp(lambda3*T)*c*a1 * Ih * Sv / Nh - lambda3 * Sv
    dEv <- exp(lambda3*T)*c*a1 * Ih * Sv / Nh - sigma2 * Ev - lambda3 * Ev
    dIv <- sigma2 * Ev - lambda3 * Iv
    
    
    return(list(c(dSh, dEh, dIh, dRh, dSv, dEv, dIv)))
  })
}

init <- c(300,0,0,0,5000,1,0)

parms <- c(mu1=0.02,upsilon=0.01,beta1=0.0465,lambda1=0.02,sigma1=0.05,
           lambda2=0,gamma=0.01, mu2=0.03, beta2=0.5488, lambda3= 0.03,
           sigma2=0.05,T=3, c= 0.025,a1=0.075, b1=0.62)
times <- seq(0, 1000, 1)

out <- as.data.frame(ode(init, times, seir, parms))

head(out)
tail(out)
min(out)
out
plot(out$`1` ~out$time, type='l', col='darkgreen', 
     xlab= 'time', ylab='N', ylim = c(0,300))
lines(out$`2`~out$time, col='red')
lines(out$`3`~out$time, col= 'blue')
lines(out$'4'~out$time, col='pink')
lines(out$'5'~out$time, col='orange')
lines(out$'6'~out$time, col='brown')
lines(out$'7'~out$time, col='black')


min(out)
max(out)
par(mfrow=c(1,2))
