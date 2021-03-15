# SAMPLE MODEL COMPARISON PLOTS
#
# SPK 03-MAR-2021
#

#Comparison of 2 Sp Model and 4 Sp Model
Two_Host_Model <- read.csv("Two-Host-Model.csv")
G_pallidipes <- read.csv("G-pallidipes.csv")

par(mfrow=c(1,2))
plot(Two_Host_Model$Inf1 ~ seq(1,1001,1), type = 'l', col = 'darkgreen', lwd = 3, ylim = c(0,200),
     ylab = "", xlab = "", axes = FALSE)
lines(G_pallidipes$Inf1 ~ seq(1,1001,1), lwd = 3, col = 'blue')
legend('topright', legend=c("2 Species", "4 Species"),
       col=c("darkgreen", "blue"), lty=1, cex=0.8, lwd = 3)
axis(1,las=1, font=2, cex.axis = 1, lwd = 2)
axis(2,las=2, font=2, cex.axis=1, lwd = 2)
mtext(side=1, line=2, "Time", col="black", font=2,cex=1.2)
mtext(side=2, line=3, "Infected Cattle", col="black", font=2, cex=1.2)


plot(Two_Host_Model$Inf1-G_pallidipes$Inf1 ~ seq(1,1001,1), type = 'l', lwd = 3,
     ylab = "",ylim=c(-60,60), xlab = "", col = 'red', axes=FALSE)
abline(h=0, lty = 2, lwd = 2)
axis(1,las=1, font=2, cex.axis = 1, lwd = 2)
axis(2,las=2, font=2, cex.axis=1, lwd = 2)
mtext(side=1, line=2, "Time", col="black", font=2,cex=1.2)
mtext(side=2, line=3, "Infected Cattle", col="black", font=2, cex=1.2)


#write.csv(out2,"~/University/4th Year/AAT-and-Wildlife/outputs/Two-Host-Model.csv")
#write.csv(out4.gswyn,"~/University/4th Year/AAT-and-Wildlife/outputs/Four-Host-Model.csv")

#Comparision of Different Tsetse Species
G_swynnertoni <- read.csv("G-swynnertoni.csv")
G_pallidipes <- read.csv("G-pallidipes.csv")
#View(G_swynnertoni)
par(mfrow=c(1,2))
plot(G_pallidipes$Inf1 ~ seq(1,1001,1),type = 'l', col = 'darkgreen', lwd=3, ylim= c(0,200),
     ylab = "Infected Cattle", xlab = "Time", axes=FALSE)
lines(G_swynnertoni$Inf1 ~ seq(1,1001,1), lwd=3, col = 'blue')
legend('bottomright', legend=c("G. pallidipes","G. swynnertoni"),
       col=c("darkgreen", "blue"), lty=1, cex=0.8, lwd=3,box.lty=0)
axis(1)
axis(2)

plot(G_pallidipes$Inf1 - G_swynnertoni$Inf1 ~ seq(1,1001,1), ylim = c(-20,20), type = 'l', lwd=3,
     ylab = "Difference between Models", xlab = "Time", col = 'red',axes=FALSE)
abline(h=0, lty = 2, lwd = 2)
axis(1)
axis(2)

#write.csv(out4.gpall,"~/University/4th Year/AAT-and-Wildlife/outputs/G-pallidipes.csv")
#write.csv(out4.gswyn,"~/University/4th Year/AAT-and-Wildlife/outputs/G-swynnertoni.csv")

#Comparison of Different Wildlife Proportions: 
out10 <- read.csv("Wildlife10.csv")
out30 <- read.csv("Wildlife30.csv")
out50 <- read.csv("Wildlife50.csv")
out70 <- read.csv("Wildlife70.csv")
out90 <- read.csv("Wildlife90.csv")
par(mfrow=c(1,1))
plot(out50$Inf1 ~ seq(1,1001,1),type='l',col='darkgreen',lwd=3,ylim=c(0,300),
     ylab = " ", xlab = "",axes=FALSE)
lines(out70$Inf1 ~ seq(1,1001,1),lwd=3, col ='blue')
lines(out90$Inf1 ~ seq(1,1001,1),lwd=3, col = 'red')
lines(out10$Inf1 ~ seq(1,1001,1),lwd=3, col = 'purple')
lines(out30$Inf1 ~ seq(1,1001,1),lwd=3, col = 'orange')
legend('topright', legend = c( "10% Wildlife","30% Wildlife", "50% Wildlife","70% Wildlife","90% Wildlife"),
       col=c("purple","orange","darkgreen","blue","red"), lty=1, cex=0.8, lwd=3)
axis(1,las=1, font=2, cex.axis = 1, lwd = 2)
axis(2,las=2, font=2, cex.axis=1, lwd = 2)
mtext(side=1, line=2, "Time", col="black", font=2,cex=1.2)
mtext(side=2, line=3, "Infected Cattle", col="black", font=2, cex=1.2)

#write.csv(out10,"~/University/4th Year/AAT-and-Wildlife/outputs/Wildlife10.csv")
#write.csv(out30,"~/University/4th Year/AAT-and-Wildlife/outputs/Wildlife30.csv")
#write.csv(out50,"~/University/4th Year/AAT-and-Wildlife/outputs/Wildlife50.csv")
#write.csv(out70,"~/University/4th Year/AAT-and-Wildlife/outputs/Wildlife70.csv")
#write.csv(out90,"~/University/4th Year/AAT-and-Wildlife/outputs/Wildlife90.csv")

#Comparison of Infection of Cattle with Increasing Wildlife Populations
par(mfrow=c(1,1))
plot(outpop0$Inf1 ~ seq(1,1001,1),type='l',col='darkgreen',lwd=3,ylim=c(0,300),
     ylab="Infected Cattle",xlab="Time")
lines(outpop10$Inf1 ~ seq(1,1001,1), lwd=3, col='blue')
lines(outpop20$Inf1~ seq(1,1001,1), lwd=3, col='red')
lines(outpop30$Inf1~ seq(1,1001,1), lwd=3, col='purple')
lines(outpop30$Inf1~ seq(1,1001,1), lwd=3, col='orange')
lines(outpop40$Inf1~ seq(1,1001,1), lwd=3, col='pink')
lines(outpop50$Inf1~ seq(1,1001,1), lwd=3, col='brown')
lines(outpop60$Inf1~ seq(1,1001,1), lwd=3, col='yellow')
lines(outpop70$Inf1~ seq(1,1001,1), lwd=3, col='lightgreen')
lines(outpop80$Inf1~ seq(1,1001,1), lwd=3, col='lightblue')
lines(outpop90$Inf1~ seq(1,1001,1), lwd=3, col='darkblue')
lines(outpop100$Inf1~ seq(1,1001,1), lwd=3, col='black')
legend('topright', legend = c("0% Increase", "10% Increase","20% Increase" , "30% Increase", "40% Increase",
                              "50% Increase","60% Increase", "70% Increase","80% Increase",
                               "90% Increase","100% Increase"),
       col=c("darkgreen","blue","red","purple","orange","pink",
             "brown", "yellow","lightgreen","lightblue","darkblue","black"),
       lty=1, cex=0.8, lwd=3)

write.csv(outpop0,"~/University/4th Year/AAT-and-Wildlife/outputs/Population0.csv")
write.csv(outpop10,"~/University/4th Year/AAT-and-Wildlife/outputs/Population10.csv")
write.csv(outpop20,"~/University/4th Year/AAT-and-Wildlife/outputs/Population20.csv")
write.csv(outpop30,"~/University/4th Year/AAT-and-Wildlife/outputs/Population30.csv")
write.csv(outpop40,"~/University/4th Year/AAT-and-Wildlife/outputs/Population40.csv")
write.csv(outpop50,"~/University/4th Year/AAT-and-Wildlife/outputs/Population50.csv")
write.csv(outpop60,"~/University/4th Year/AAT-and-Wildlife/outputs/Population60.csv")
write.csv(outpop70,"~/University/4th Year/AAT-and-Wildlife/outputs/Population70.csv")
write.csv(outpop80,"~/University/4th Year/AAT-and-Wildlife/outputs/Population80.csv")
write.csv(outpop90,"~/University/4th Year/AAT-and-Wildlife/outputs/Population90.csv")
write.csv(outpop100,"~/University/4th Year/AAT-and-Wildlife/outputs/Population100.csv")

#Sensitivity Testing of Tsetse Species at Three Different Wildlife Proportions
Gpall10 <- read.csv("Gpall10.csv")
Gpall50 <- read.csv("Gpall50.csv")
Gpall90 <- read.csv("Gpall90.csv")
Gswyn10 <- read.csv("Gswyn10.csv")
Gswyn50 <- read.csv("Gswyn50.csv")
Gswyn90 <- read.csv("Gswyn90.csv")
par(mfrow=c(1,3))
plot(Gpall10$Inf1 ~ seq(1,1001,1),type='l',col='darkgreen',lwd=3,ylim=c(0,200),
     ylab=" ",xlab="",main="10% Wildlife Proportion",axes=FALSE)
lines(Gswyn10$Inf1 ~ seq(1,1001,1), lwd=3, col='blue')
legend('topright', legend=c("G. pallidipes","G. swynnertoni"),
       col=c("darkgreen", "blue"), lty=1, cex=1, lwd=3)
axis(1,las=1, font=2, cex.axis = 1.1, lwd = 2)
axis(2,las=2, font=2, cex.axis=1.1, lwd = 2)
mtext(side=1, line=2, "Time", col="black", font=2,cex=1)
mtext(side=2, line=3, "Infected Cattle", col="black", font=2, cex=1)

plot(Gpall50$Inf1 ~ seq(1,1001,1),type='l', col='darkgreen',lwd=3,ylim=c(0,200),#
     ylab=" ",xlab="",main="50% Wildlife Proportion",axes=FALSE)
lines(Gswyn50$Inf1 ~ seq(1,1001,1),lwd=3, col='blue')
legend('topright', legend=c("G. pallidipes","G. swynnertoni"),
       col=c("darkgreen", "blue"), lty=1, cex=1, lwd=3)
axis(1,las=1, font=2, cex.axis = 1.1, lwd = 2)
axis(2,las=2, font=2, cex.axis=1.1, lwd = 2)
mtext(side=1, line=2, "Time", col="black", font=2,cex=1)
mtext(side=2, line=3, "Infected Cattle", col="black", font=2, cex=1)

plot(Gpall90$Inf1 ~ seq(1,1001,1),type='l', col='darkgreen',lwd=3,ylim=c(0,200),#
     ylab=" ",xlab="",main="90% Wildlife Proportion",axes=FALSE)
lines(Gswyn90$Inf1 ~ seq(1,1001,1),lwd=3, col='blue')
legend('topright', legend=c("G. pallidipes","G. swynnertoni"),
       col=c("darkgreen", "blue"), lty=1, cex=1, lwd=3)
axis(1,las=1, font=2, cex.axis = 1.1, lwd = 2)
axis(2,las=2, font=2, cex.axis=1.1, lwd = 2)
mtext(side=1, line=2, "Time", col="black", font=2,cex=1)
mtext(side=2, line=3, "Infected Cattle", col="black", font=2, cex=1)

#Comparison of the Tsetse Species: 
Gpall10 <- read_csv("Gpall10.csv")
Gpall50 <- read_csv("Gpall50.csv")
Gpall90 <- read_csv("Gpall90.csv")
Gswyn10 <- read_csv("Gswyn10.csv")
Gswyn50 <- read_csv("Gswyn50.csv")
Gswyn90 <- read_csv("Gswyn90.csv")
par(mfrow=c(1,3))
plot(Gpall10$Inf1 - Gswyn10$Inf1 ~ seq(1,1001,1), ylim = c(-10,10), type = 'l', lwd=3,
     ylab = "", xlab = "", col = 'red',main="10% Wildlife Proportion",
     axes=FALSE)
abline(h=0, lty = 2, lwd = 2)
axis(1,las=1, font=2, cex.axis = 1.1, lwd = 2)
axis(2,las=2, font=2, cex.axis=1.1, lwd = 2)
mtext(side=1, line=2, "Time", col="black", font=2,cex=1)
mtext(side=2, line=3, "Infected Cattle", col="black", font=2, cex=1)

plot(Gpall50$Inf1 - Gswyn50$Inf1 ~ seq(1,1001,1), ylim = c(-10,10), type = 'l', lwd=3,
     ylab = "", xlab = "", col = 'red',main="50% Wildlife Proportion",
     axes=FALSE)
abline(h=0, lty = 2, lwd = 2)
axis(1,las=1, font=2, cex.axis = 1.1, lwd = 2)
axis(2,las=2, font=2, cex.axis=1.1, lwd = 2)
mtext(side=1, line=2, "Time", col="black", font=2,cex=1)
mtext(side=2, line=3, "Infected Cattle", col="black", font=2, cex=1)

plot(Gpall90$Inf1 - Gswyn90$Inf1 ~ seq(1,1001,1), ylim = c(-10,10), type = 'l', lwd=3,
     ylab = "", xlab = "", col = 'red',main="90% Wildlife Proportion",
     axes=FALSE)
abline(h=0, lty = 2, lwd = 2)
axis(1,las=1, font=2, cex.axis = 1.1, lwd = 2)
axis(2,las=2, font=2, cex.axis=1.1, lwd = 2)
mtext(side=1, line=2, "Time", col="black", font=2,cex=1)
mtext(side=2, line=3, "Infected Cattle", col="black", font=2, cex=1)

#write.csv(out4.gpall10,"~/University/4th Year/AAT-and-Wildlife/outputs/Gpall10.csv")
#write.csv(out4.gpall50,"~/University/4th Year/AAT-and-Wildlife/outputs/Gpall50.csv")
#write.csv(out4.gpall90,"~/University/4th Year/AAT-and-Wildlife/outputs/Gpall90.csv")
#write.csv(out4.gswyn10,"~/University/4th Year/AAT-and-Wildlife/outputs/Gswyn10.csv")
#write.csv(out4.gswyn50,"~/University/4th Year/AAT-and-Wildlife/outputs/Gswyn50.csv")
#write.csv(out4.gswyn90,"~/University/4th Year/AAT-and-Wildlife/outputs/Gswyn90.csv")

#Sensitivity Testing of Wildlife Increases at Differnet Wildlife Proportions 
outpop0 <- read.csv("Population0.csv")
outpop10 <-read.csv("Population10.csv")
outpop20 <-read.csv("Population20.csv")
outpop30 <-read.csv("Population30.csv")
outpop40 <- read.csv("Population40.csv")
outpop50 <- read.csv("Population50.csv")
outpop60 <- read.csv("Population60.csv")
outpop70 <- read.csv("Population70.csv")
outpop80 <- read.csv("Population80.csv")
outpop90 <- read.csv("Population90.csv")
outpop100 <- read.csv("Population100.csv")

par(mfrow=c(1,1))
plot(outpop0$Inf1 ~ seq(1,1001,1),type='l',col='darkgreen',lwd=3,ylim=c(0,300),
     ylab="Infected Cattle",xlab="Time")
lines(outpop10$Inf1 ~ seq(1,1001,1), lwd=3, col='blue')
lines(outpop20$Inf1~ seq(1,1001,1), lwd=3, col='red')
lines(outpop30$Inf1~ seq(1,1001,1), lwd=3, col='purple')
lines(outpop30$Inf1~ seq(1,1001,1), lwd=3, col='orange')
lines(outpop40$Inf1~ seq(1,1001,1), lwd=3, col='pink')
lines(outpop50$Inf1~ seq(1,1001,1), lwd=3, col='brown')
lines(outpop60$Inf1~ seq(1,1001,1), lwd=3, col='yellow')
lines(outpop70$Inf1~ seq(1,1001,1), lwd=3, col='lightgreen')
lines(outpop80$Inf1~ seq(1,1001,1), lwd=3, col='lightblue')
lines(outpop90$Inf1~ seq(1,1001,1), lwd=3, col='darkblue')
lines(outpop100$Inf1~ seq(1,1001,1), lwd=3, col='black')
legend('topright', legend = c("0% Increase", "10% Increase","20% Increase" , "30% Increase", "40% Increase",
                              "50% Increase","60% Increase", "70% Increase","80% Increase",
                              "90% Increase","100% Increase"),
       col=c("darkgreen","blue","red","purple","orange","pink",
             "brown", "yellow","lightgreen","lightblue","darkblue","black"),
       lty=1, cex=0.8, lwd=3)

Wildlife_Populations_10 <- read.csv("Wildlife Populations_10.csv")
Wildlife_Populations_50 <- read.csv("Wildlife Populations_50.csv")
Wildlife_Populations_90 <- read.csv("Wildlife Populations_90.csv")
plot(Wildlife_Populations_10$V1~ seq(1,1001,1),type='l',col='darkgreen',lwd=3,ylim=c(0,300),
     ylab="Infected Cattle",xlab="Time")
lines(outpop10$Inf1 ~ seq(1,1001,1), lwd=3, col='blue')


#Code for Altering the Rplots 
axis(1)
axis(2, at = c(0,50,100,150,200))
Axes = FALSE
