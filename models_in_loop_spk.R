# This is an example of running models and plots on a loop
# 
# SPK 05-MAR-2021
#
#



lower <- 0.1
upper <- 0.9
by <- 0.1
range <- seq(lower, upper, by)
length(range)
mat <- matrix(1:(1001*(length(range)+1)), ncol = length(range)+1)
mat[,] <- NA
mat[,length(range)+1] <- seq(1,1001,1)

for (i in 1:length(range)){
  #To calculate host population sizes, 
  #wildlife density from Auty et al,2016 are used for the three main wildlife hosts:
  #buffalo (7.78 animals per km2)giraffe (4.07 animals per km2), elephant (2.4 animals per km2)
  #cattle density from Lord et al, 2020 are used for cattle (30 animals per km2)
  
  #From Auty etl al, 2016, cattle graze an area of 0-22km in the dry season (mean 3.47, median 2.5)
  #this is used as a diameter to then calculate the area in which the cattle travel:
  
  cat.graz.dis <- 3.47
  area <- pi * (cat.graz.dis/2)^2 #kilometres squared
  cat.pop <- 30   #average number of cattle per km2
  buf.pop <- 7.78 #average number of buffalo per km2
  gir.pop <- 4.07 #average number of giraffes per km2
  ele.pop <- 2.4  #average number of elephants per km2
  
  
  cat.dens <- cat.pop * area
  buf.dens <- buf.pop * area
  gir.dens <- gir.pop * area
  ele.dens <- ele.pop * area
  
  cat.adj.dens <- round(cat.dens, digits=0)
  buf.adj.dens <- round(buf.dens,digits=0) 
  gir.adj.dens <- round(gir.dens,digits=0) 
  ele.adj.dens <- round(ele.dens,digits=0) 
  
  wildlife.dens <- buf.adj.dens + gir.adj.dens + ele.adj.dens
  
  init2 <- c(Sus1 = cat.adj.dens,    #susceptible host 1
             Exp1 = 0,                #exposed host 1
             Inf1 = 0,                #infected host 1
             Rec1 = 0,                #recovered host 1
             SusV=5000,               #susceptible tsetse vector
             ExpV=0,                  #exposed tsetse vector 
             InfV=1,                  #infected tsetse vector
             Sus2= wildlife.dens,      #susceptible host 2
             Exp2=0,                  #exposed host 2
             Inf2=0,                  #infected host 2
             Rec2=0)                  #recovered host 2.
  
  
  #To calculate the proportion of bloodmeals from different wildlife host,
  #takem from Auty et al, 2016, the three main hosts of tsetse bloodmeals
  #are buffalo (57%), giraffe (20%) and elephant (11%)
  #At the park boundary we assume that 50% of bloodmeals will be from
  #wildlife and 50% from livestock and humans
  
  wildlife.prop <- range[i]
  buf <- 0.57
  gir <- 0.2
  ele <- 0.11
  
  buf.adj <- buf / (buf + gir + ele) * wildlife.prop
  gir.adj <- gir/(buf + gir + ele) * wildlife.prop
  ele.adj <- ele/(buf + gir + ele) * wildlife.prop
  
  ifelse(buf.adj + gir.adj + ele.adj == wildlife.prop, "TRUE", "FALSE")
  
  domestic.prop <- 1 - wildlife.prop
  hum <- 0.3
  cat <- 0.7
  
  hum.adj <- hum / (hum + cat) * domestic.prop
  cat.adj <- cat / (hum +cat) * domestic.prop
  
  ifelse(hum.adj + cat.adj == domestic.prop,"TRUE", "FALSE")
  
  #weighted averages of the three main wildlife species
  wildlife.av <- ((buf.adj*buf)+(gir.adj*gir)+(ele.adj*ele))/3
  
  #To calculate values for 'a' in the parameters, the above values
  #are divided by duration of feeding cycles in tsetse = d
  d <- 4
  
  parms2 <- c(
    # host 1 = cattle
    mu1=0,        #birth rate of host 1
    lambda1=0,    #natural death rate of host 1
    sigma1=0.05,  #rate of infected to infectious in host 1
    lambda2=0,    #death due to infection in host 1
    gamma1=0.01,  #recovery rate of host 1
    a1=cat.adj/d,     #portion of tsetse bloodmeals/duration of feeding cycles in fly
    #for host 1
    b1=0.62,      #probability of infected fly bite giving rise to infection in host 1
    upsilon1=0.01,#rate of waning immunity in host 1
    
    # host 2 = wildlife
    mu3= 0,        #birth rate of host 2
    lambda4=0,     #natural death rate of host 2
    sigma3=0.05,   #rate of infected to infectious in host 2
    upsilon2=0.01, #rate of waning immunity in host 2
    lambda5=0,     #death dye to infection in host 2
    gamma2=0.01,   #recovery rate in host 2
    a2=wildlife.av/d,      #portion of tsetse bloodmeals/duration of feeding cycles in fly
    #for host 2
    b2=0.62,       #probability of infected fly bite giving rise to infection in host 2
    
    # vector
    mu2=     0.01,         #birth rate of vector
    lambda3= 0.01,    #natural death rate of vector
    sigma2=0.05,   #rate of infected to infectious in vector 
    T=3,           #incubation period
    c= 0.025)      #probability of infected bloodmeal giving rise to infection in fly
  times2 <- seq(0, 1000, 1)
  
  out2 <- as.data.frame(ode(init2, times2, seir2, parms2))
  
  mat[,i] <- out2[,4]
}

par(mfrow=c(1,1))

library(randomcoloR)

cols <- c("black","red","green", "purple", "orange", "blue", "brown", "darkred", "darkgreen")
 # col = cols[j]
plot(mat[,1] ~ mat[,length(range)+1], ylim = c(0,190), type = 'l')
for (j in 2:length(range)){
  lines(mat[,j] ~ mat[,length(range)+1], col = randomColor(1))
}






