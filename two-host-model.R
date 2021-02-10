#Load package 
library(deSolve)

#SEIR Model for Two Vertebrate Hosts and One Vector Model 
seir <- function(times, init, parameters) {
  
  Sh1 <- init[1] #susceptible host species 1
  Eh1 <- init[2] #exposed host species 1
  Ih1 <- init[3] #infected host species 1
  Rh1 <- init[4] #recovered host species 1
  Nh1 <- Sh1 + Eh1 + Ih1 + Rh1 #total population of host species 1
  
  Sh2 <- init[8] #susceptible host species 2
  Eh2 <- init[9] #exposed host species 2
  Ih2 <- init[10] #infected host species 2
  Rh2 <- init[11] #recovered host species 2
  Nh2 <- Sh2 + Eh2 + Ih2 + Rh2 #total population of host species 2
  
  Sv <- init [5] #susceptible vector
  Ev <- init [6] #exposed vector
  Iv <- init [7] #infected vector
  Vv <- Sv + Ev + Iv #total popuation of vector
  #assumed that vectors remain infected for life
  
  with(as.list(c(parameters)), {
    
    #mu1 = birth rate of host species 1
    #upsilon1 = rate of waning immunity of host species 1
    #lambda1 = natural death rate of host species 1 
    #sigma1 = rate of infected to infectious in host species 1
    #lambda2 = death rate due to infection in host species 1
    #gamma1 = rate of recovery in host species 1
    #a1 = portion of tsetse bloodmeals/duration of feeding cycles in fly
    #     for host species 1
    #b1 = probability of infected fly bite giving rise to 
    #     infection in host species 1
    
    dSh1 <- mu1 * Nh1 + upsilon1 * Rh1 - a1*b1 * Sh1 * Iv/Nh1 - lambda1 * Sh1
    dEh1 <- a1*b1 * Sh1 * Iv /Nh1 - sigma1 * Eh1 - lambda1 * Eh1
    dIh1 <- sigma1 * Eh1 - lambda1 * Ih1 - lambda2 * Ih1 - gamma1 * Ih1
    dRh1 <- gamma1 * Ih1 - lambda1 * Rh1 - upsilon1 * Rh1
    
    #mu3 = birth rate of host species 2
    #upsilon2 = rate of waning immmunity of host species 2
    #lambda4 = natural death rate of host species 2
    #sigma3 = rate of infected to infectious in host species 2
    #lambda5 = death rate due to infection in host species 2
    #gamma2 = rate of recovery in host species 2 
    #a2 = propotion of tsetse bloodmeals/duration of feeding cycles in fly
    #     for host species 2
    #b2 = probability of infected fly bite giving rise to 
    #     infection in host species 2
    
    dSh2 <- mu3 * Nh2 + upsilon2 * Rh2 - a2*b2 *Sh2 *Iv/Nh2 - lambda4 *Sh2
    dEh2 <- a2*b2 *Sh2 *Iv /Nh2 - sigma3 * Eh2 - lambda4 *Eh2
    dIh2 <- sigma3 * Eh2 - lambda4 * Ih2 - lambda5 *Ih2 - gamma2 *Ih2
    dRh2 <- gamma2 * Ih2 - lambda4 * Rh2 - upsilon2 * Rh2
    
    #mu2 = birth rate of vector
    #lambda3 = natural death rate of vector
    #sigma2 = rate of infected to infectious in vector 
    #T= incubation period
    #exp = probability of survival of vector
    #c = probability of infected bloodmeal giving rise to infection in fly
    
    dSv <- mu2 * Vv - exp(-lambda3*T)*(c*a1*Ih1*Sv/Nh1 + c*a2*Ih2*Sv/Nh2) - lambda3 * Sv
    dEv <- exp(-lambda3*T)*(c*a1*Ih1*Sv/Nh1 + c*a2*Ih2*Sv/Nh2) - sigma2 * Ev - lambda3 * Ev
    dIv <- sigma2 * Ev - lambda3 * Iv
    
    
    return(list(c(dSh1, dEh1, dIh1, dRh1, dSv, dEv, dIv,dSh2,dEh2,dIh2,dRh2)))
  })
}

init <- c(300,0,0,0,5000,0,1,300,0,0,0)

# SPK: Have reorganised this to be more aestetically pleasing and easy to read. 
#      You should go through and add labels to each as you have elsewhere for 
#      clarity.

parms <- c(
           # host 1
           mu1=0,
           lambda1=0,
           sigma1=0.05,
           lambda2=0,
           gamma1=0.01,
           a1=0.075,
           b1=0.62,
           upsilon1=0.01,
           
           # host 2
           mu3= 0,
           lambda4=0,
           sigma3=0.05,
           upsilon2=0.01,
           lambda5=0, 
           gamma2=0.01, 
           a2=0.075, 
           b2=0.62,
           
           # vector
           mu2=0, 
           lambda3= 0,
           sigma2=0.05,
           T=3, 
           c= 0.025)
times <- seq(0, 1000, 1)

out <- as.data.frame(ode(init, times, seir, parms))

head(out)
tail(out)
min(out)


par(mfrow=c(1,3)) # To include 3 plots on the one row

# SPK: Have added a third plot to show the second host species. Have also tweaked
#      the colours slightly to be consistent and I think clear what is what for 
#      now:
#          - Susceptible: Dark Green
#          - Exposed: Orange
#          - Infected: Red
#          - Recovered: Blue
#      Have also added some plotting features to make the lines more clear. This
#      is the "lwd =" command within the plot and line brackets. 
#      

# Host 1
plot(out$`1` ~out$time, type='l', col='darkgreen', 
     xlab= 'time', ylab='N', ylim = c(0,300), lwd = 2)
lines(out$`2`~out$time, col='orange', lwd = 2)
lines(out$`3`~out$time, col= 'red', lwd = 2)
lines(out$'4'~out$time, col='blue', lwd = 2)

# Host 2
plot(out$`8` ~out$time, type='l', col='darkgreen', 
     xlab= 'time', ylab='N', ylim = c(0,300), lwd = 2)
lines(out$`9`~out$time, col='orange', lwd = 2)
lines(out$`10`~out$time, col= 'red', lwd = 2)
lines(out$'11'~out$time, col='blue', lwd = 2)

# Vector
plot(out$`5` ~out$time, type='l', col='darkgreen', 
     xlab= 'time', ylab='N', ylim = c(0,5000), lwd = 2)
lines(out$'6'~out$time, col='orange', lwd = 2)
lines(out$'7'~out$time, col='red', lwd = 2)


min(out)
max(out)
