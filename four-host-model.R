#Load package 
library(deSolve)

#SEIR Model for Two Vertebrate Hosts and One Vector Model 
seir4 <- function(times, init, parameters) {
  
  Sh1 <- init[1]               #susceptible host species 1
  Eh1 <- init[2]               #exposed host species 1
  Ih1 <- init[3]               #infected host species 1
  Rh1 <- init[4]               #recovered host species 1
  Nh1 <- Sh1 + Eh1 + Ih1 + Rh1 #total population of host species 1
  
  Sh2 <- init[8]               #susceptible host species 2
  Eh2 <- init[9]               #exposed host species 2
  Ih2 <- init[10]              #infected host species 2
  Rh2 <- init[11]              #recovered host species 2
  Nh2 <- Sh2 + Eh2 + Ih2 + Rh2 #total population of host species 2
  
  Sh3 <- init[12]              #susceptibel host species 3
  Eh3 <- init[13]              #exposed host species 3
  Ih3 <- init[14]              #infected host species 3
  Rh3 <- init[15]              #recovered host species 3
  Nh3 <- Sh3 + Eh3 + Ih3 + Rh3 #total population of host species 3
  
  Sh4 <- init[16]              #susceptible host species 4
  Eh4 <- init[17]              #exposed host species 4
  Ih4 <- init[18]              #infected host species 4
  Rh4 <- init[19]              #recovered host species 4
  Nh4 <- Sh4 + Eh4 + Ih4 + Rh4 #total population of host species 4
  
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
    
    #mu4 = birth rate of host 3
    #upsilon3= rate of waning immunity in host 3
    #a3= proportion of tsetse bloodmeals/duration of feeding cycles in fly
    #    for host 3
    #b3= probability of infected fly bite giving rise to infection in host 3
    #lambda6= natural death rate of host 3
    #sigma4= rate of infected to infectious in host 3
    #lambda7= death rate due to infection in host 3
    #gamma3= recovery rate in host 3
    
    dSh3 <- mu4 * Nh3 + upsilon3 * Rh3 - a3*b3 * Sh3 * Iv/Nh3 - lambda6 * Sh3
    dEh3 <- a3*b3 * Sh3 * Iv /Nh3 - sigma4 * Eh3 - lambda6 * Eh3
    dIh3 <- sigma4 * Eh4 - lambda6 * Ih3 - lambda7 * Ih3 - gamma3 * Ih3
    dRh3 <- gamma3 * Ih3 - lambda6 * Rh3 - upsilon3 * Rh3
    
    #mu5= birth rate of host 4
    #upsilon4= rate of waning immunity in host 4
    #a4= proportion of tsetse bloodmeals/duration of feeding cycles in fly
    #    for host 4
    #b4= probability of infected fly bite giving rise to infection in host 4
    #lambda8= natural death rate of host 4
    #sigma5= rate of infected to infectious in host 4
    #lambda9= death rate due to infection in host 4
    #gamma4= recovery rate in host 4 
    
    dSh4 <- mu5 * Nh4 + upsilon4 * Rh4 - a4*b4 * Sh4 * Iv/Nh4 - lambda8 * Sh4
    dEh4 <- a4*b4 * Sh4 * Iv /Nh4 - sigma5 * Eh4 - lambda8 * Eh4
    dIh4 <- sigma5 * Eh4 - lambda8 * Ih4 - lambda9 * Ih4 - gamma4 * Ih4
    dRh4 <- gamma4 * Ih4 - lambda8 * Rh4 - upsilon4 * Rh4
    
    #mu2 = birth rate of vector
    #lambda3 = natural death rate of vector
    #sigma2 = rate of infected to infectious in vector 
    #T= incubation period
    #exp = probability of survival of vector
    #c = probability of infected bloodmeal giving rise to infection in fly
    
    dSv <- mu2 * Vv - exp(-lambda3*T)*(c*a1*Ih1*Sv/Nh1 + c*a2*Ih2*Sv/Nh2 
                                       +c*a3*Ih3*Sv/Nh3 +c*a4*Ih4*Sv/Nh4) - lambda3 * Sv
    dEv <- exp(-lambda3*T)*(c*a1*Ih1*Sv/Nh1 + c*a2*Ih2*Sv/Nh2
                            +c*a3*Ih3*Sv/Nh3 + c*a4*Ih4*Sv/Nh4) - sigma2 * Ev - lambda3 * Ev
    dIv <- sigma2 * Ev - lambda3 * Iv
    
    
    return(list(c(dSh1, dEh1, dIh1, dRh1, dSv, dEv, dIv,dSh2,dEh2,dIh2,dRh2,dSh3,dEh3,
                  dIh3,dRh3,dSh4,dEh4,dIh4,dRh4)))
  })
}

init4 <- c(Sus1 = 300,    #susceptible host 1
          Exp1 = 0,      #exposed host 1
          Inf1 = 0,      #infected host 1
          Rec1 = 0,      #recovered host 1
          SusV=5000,     #susceptible tsetse vector
          ExpV=0,        #exposed tsetse vector 
          InfV=1,        #infected tsetse vector
          Sus2=300,      #susceptible host 2
          Exp2=0,        #exposed host 2
          Inf2=0,        #infected host 2
          Rec2=0,        #recovered host 2.
          Sus3=300,      #susceptible host 3
          Exp3=0,        #exposed host 3
          Inf3=0,        #infected host 3
          Rec3=0,        #recovered host 3
          Sus4=300,        #susceptible host 4
          Exp4=0,        #exposed host 4
          Inf4=0,        #infected host 4
          Rec4=0)        #recovered host 4

# SPK: Have reorganised this to be more aestetically pleasing and easy to read. 
#      You should go through and add labels to each as you have elsewhere for 
#      clarity.

#To calculate the proportion of bloodmeals from different wildlife host,
#takem from Auty et al, 2016, the three main hosts of tsetse bloodmeals
#are buffalo (57%), giraffe (20%) and elephant (11%)
#At the park boundary we assume that 50% of bloodmeals will be from
#wildlife and 50% from livestock and humans

wildlife.prop <- 0.5
buf <- 0.57
gir <- 0.2
ele <- 0.11

buf.adj <- buf / (buf + gir + ele) * wildlife.prop
gir.adj <- gir/(buf + gir + ele) * wildlife.prop
ele.adj <- ele/(buf + gir + ele) * wildlife.prop

ifelse(buf.adj + gir.adj + ele.adj == wildlife.prop, "TRUE", "FALSE")

domestic.prop <- 1-wildlife.prop
hum <- 0.3
cat <- 0.7

hum.adj <- hum / (hum + cat) * domestic.prop
cat.adj <- cat / (hum +cat) * domestic.prop

ifelse(hum.adj + cat.adj == domestic.prop,"TRUE", "FALSE")

#To calculate values for 'a' in the parameters, the above values
#are divided by duration of feeding cycles in tsetse = d
d <- 4

parms4 <- c(
  # host 1 = cattle
  mu1=0,        #birth rate of host 1
  lambda1=0,    #natural death rate of host 1
  sigma1=0.067,  #rate of infected to infectious in host 1
  lambda2=0,    #death due to infection in host 1
  gamma1=0.01,  #recovery rate of host 1
  a1=cat.adj/d,     #portion of tsetse bloodmeals/duration of feeding cycles in fly
  #for host 1
  b1=0.46,      #probability of infected fly bite giving rise to infection in host 1
  upsilon1=0.01,#rate of waning immunity in host 1
  
  # host 2 = buffalo
  mu3= 0,        #birth rate of host 2
  lambda4=0,     #natural death rate of host 2
  sigma3=0.067,   #rate of infected to infectious in host 2
  upsilon2=0.01, #rate of waning immunity in host 2
  lambda5=0,     #death dye to infection in host 2
  gamma2=0.0042,   #recovery rate in host 2
  a2=buf.adj/d,      #portion of tsetse bloodmeals/duration of feeding cycles in fly
                 #for host 2
  b2=0.46,       #probability of infected fly bite giving rise to infection in host 2
  
  # host 3 = giraffe
  mu4 = 0,       #birth rate of host 3
  upsilon3= 0.01,#rate of waning immunity in host 3
  a3= gir.adj/d,     #proportion of tsetse bloodmeals/duration of feeding cycles in fly
                 #for host 3
  b3=0.46,       #probability of infected fly bite giving rise to infection in host 3
  lambda6=0,     #natural death rate of host 3
  sigma4=0.067,   #rate of infected to infectious in host 3
  lambda7=0,     #death rate due to infection in host 3
  gamma3= 0.0042,  #recovery rate in host 3
  
  # host 4 = elephant
  mu5=0,         #birth rate of host 4
  upsilon4= 0.01,#rate of waning immunity in host 4
  a4= ele.adj/d,     #proportion of tsetse bloodmeals/duration of feeding cycles in fly
                 #for host 4
  b4=0.46,       #probability of infected fly bite giving rise to infection in host 4
  lambda8=0,     #natural death rate of host 4
  sigma5=0.067,   #rate of infected to infectious in host 4
  lambda9=0,     #death rate due to infection in host 4
  gamma4=0.0042,   #recovery rate in host 4 
  
  # vector = tsetse G. pallidipes
  mu2=     0.01,         #birth rate of vector
  lambda3= 0.01,    #natural death rate of vector
  sigma2=0.05,   #rate of infected to infectious in vector 
  T=3,           #incubation period
  c= 0.025)      #probability of infected bloodmeal giving rise to infection in fly
times4 <- seq(0, 1000, 1)

out4 <- as.data.frame(ode(init, times, seir, parms))

head(out4)
tail(out4)
min(out4)


par(mfrow=c(2,3)) # To include 3 plots on the one row

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
plot(out$Sus1 ~ out4$time, type='l', col='darkgreen', 
     xlab= 'time', ylab='N', ylim = c(0,300), lwd = 2)
lines(out4$Exp1 ~ out4$time, col='orange', lwd = 2)
lines(out4$Inf1 ~ out4$time, col= 'red', lwd = 2)
lines(out4$Rec1 ~ out4$time, col='blue', lwd = 2)
legend(500, 300, legend=c("Sus1", "Exp1", "Inf1", "Rec1"),
       col=c("darkgreen", "orange","red","blue"), lty=1, cex=0.8)


# plot(out$`1` ~out$time, type='l', col='darkgreen', 
#      xlab= 'time', ylab='N', ylim = c(0,300), lwd = 2)
# lines(out$`2`~out$time, col='orange', lwd = 2)
# lines(out$`3`~out$time, col= 'red', lwd = 2)
# lines(out$'4'~out$time, col='blue', lwd = 2)

# Host 2
plot(out4$`Sus2` ~out4$time, type='l', col='darkgreen', 
     xlab= 'time', ylab='N', ylim = c(0,300), lwd = 2)
lines(out4$`Exp2`~out4$time, col='orange', lwd = 2)
lines(out4$`Inf2`~out4$time, col= 'red', lwd = 2)
lines(out4$'Rec2'~out4$time, col='blue', lwd = 2)
legend(500, 300, legend=c("Sus2", "Exp2", "Inf2", "Rec2"),
       col=c("darkgreen", "orange","red", "blue"), lty=1, cex=0.8)
#Host 3
plot(out4$`Sus3` ~out4$time, type='l', col='darkgreen', 
     xlab= 'time', ylab='N', ylim = c(0,300), lwd = 2)
lines(out4$`Exp3`~out4$time, col='orange', lwd = 2)
lines(out4$`Inf3`~out4$time, col= 'red', lwd = 2)
lines(out4$'Rec3'~out4$time, col='blue', lwd = 2)
legend(500, 300, legend=c("Sus3", "Exp3", "Inf3", "Rec3"),
       col=c("darkgreen", "orange","red", "blue"), lty=1, cex=0.8)
#Host 4
plot(out4$`Sus4` ~out4$time, type='l', col='darkgreen', 
     xlab= 'time', ylab='N', ylim = c(0,300), lwd = 2)
lines(out4$`Exp4`~out4$time, col='orange', lwd = 2)
lines(out4$`Inf4`~out4$time, col= 'red', lwd = 2)
lines(out4$'Rec4'~out4$time, col='blue', lwd = 2)
legend(500, 300, legend=c("Sus4", "Exp4", "Inf4", "Rec4"),
       col=c("darkgreen", "orange","red", "blue"), lty=1, cex=0.8)

# Vector
plot(out4$`SusV` ~out4$time, type='l', col='darkgreen', 
     xlab= 'time', ylab='N', ylim = c(0,5000), lwd = 2)
lines(out4$'ExpV'~out4$time, col='orange', lwd = 2)
lines(out4$'InfV'~out4$time, col='red', lwd = 2)
legend(500, 5000, legend=c("SusV", "ExpV", "InfV"),
       col=c("darkgreen", "orange","red"), lty=1, cex=0.8)

min(out4)
max(out4)  

out4$Total1 <- out$Sus1 + out$Exp1 + out$Inf1 + out$Rec1
out4$Total2 <- out$Sus2 + out$Exp2 + out$Inf2 + out$Rec2
out4$Total3 <- out$Sus3 + out$Exp3 + out$Inf3 + out$Rec3
out4$Total4 <- out$Sus4 + out$Exp4 + out$Inf4 + out$Rec4
out4$TotalV <- out$SusV + out$ExpV + out$InfV
View(out4)
