# SAMPLE MODEL COMPARISON PLOTS
#
# SPK 03-MAR-2021
#

#Comparison of 2 Sp Model and 4 Sp Model
par(mfrow=c(1,2))
plot(out2$Inf1 ~ seq(1,1001,1), type = 'l', col = 'darkgreen', lwd = 3, ylim = c(0,200),
     ylab = "Infected Cattle", xlab = "Time")
lines(out4.gswyn$Inf1 ~ seq(1,1001,1), lwd = 3, col = 'blue')
legend('topright', legend=c("2 Species", "4 Species"),
       col=c("darkgreen", "blue"), lty=1, cex=0.8, lwd = 3)

plot(out2$Inf1-out4.gswyn$Inf1 ~ seq(1,1001,1), ylim = c(-60,60), type = 'l', lwd = 3,
     ylab = "Difference between 2 Sp Model & 4 Sp Model", xlab = "Time", col = 'red')
abline(h=0, lty = 2, lwd = 2)

write.csv(out2,"~/University/4th Year/AAT-and-Wildlife/outputs/Two-Host-Model.csv")
write.csv(out4.gswyn,"~/University/4th Year/AAT-and-Wildlife/outputs/Four-Host-Model.csv")

#Comparision of Different Tsetse Species
par(mfrow=c(1,2))
plot(out4.gpall$Inf1 ~ seq(1,1001,1),type = 'l', col = 'darkgreen', lwd=3, ylim= c(0,200),
     ylab = "Infected Cattle", xlab = "Time")
lines(out4.gswyn$Inf1 ~ seq(1,1001,1), lwd=3, col = 'blue')
legend('topright', legend=c("G. pallidipes","G. swynnertoni"),
       col=c("darkgreen", "blue"), lty=1, cex=0.8, lwd=3)

plot(out4.gpall$Inf1 - out4.gswyn$Inf1 ~ seq(1,1001,1), ylim = c(-60,60), type = 'l', lwd=3,
     ylab = "Difference between Tsetse Species", xlab = "Time", col = 'red')
abline(h=0, lty = 2, lwd = 2)

write.csv(out4.gpall,"~/University/4th Year/AAT-and-Wildlife/outputs/G-pallidipes.csv")
write.csv(out4.gswyn,"~/University/4th Year/AAT-and-Wildlife/outputs/G-swynnertoni.csv")

#Comparison of Different Wildlife Proportions: 
par(mfrow=c(1,1))
plot(out50$Inf1 ~ seq(1,1001,1),type='l',col='darkgreen',lwd=3,ylim=c(0,300),
     ylab = "Infected Cattle", xlab = "Time")
lines(out70$Inf1 ~ seq(1,1001,1),lwd=3, col ='blue')
lines(out90$Inf1 ~ seq(1,1001,1),lwd=3, col = 'red')
lines(out10$Inf1 ~ seq(1,1001,1),lwd=3, col = 'purple')
lines(out30$Inf1 ~ seq(1,1001,1),lwd=3, col = 'orange')
legend('topright', legend = c( "10% Wildlife","30% Wildlife", "50% Wildlife","70% Wildlife","90% Wildlife"),
       col=c("purple","orange","darkgreen","blue","red"), lty=1, cex=0.8, lwd=3)

write.csv(out10,"~/University/4th Year/AAT-and-Wildlife/outputs/Wildlife10.csv")
write.csv(out30,"~/University/4th Year/AAT-and-Wildlife/outputs/Wildlife30.csv")
write.csv(out50,"~/University/4th Year/AAT-and-Wildlife/outputs/Wildlife50.csv")
write.csv(out70,"~/University/4th Year/AAT-and-Wildlife/outputs/Wildlife70.csv")
write.csv(out90,"~/University/4th Year/AAT-and-Wildlife/outputs/Wildlife90.csv")

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
par(mfrow=c(1,3))
plot(out4.gpall10$Inf1 ~ seq(1,1001,1),type='l',col='darkgreen',lwd=3,ylim=c(0,300),
     ylab="Infected Cattle",xlab="Time",main="10% Wildlife")
lines(out4.gswyn10$Inf1 ~ seq(1,1001,1), lwd=3, col='blue')
legend('topright', legend=c("G. pallidipes","G. swynnertoni"),
       col=c("darkgreen", "blue"), lty=1, cex=0.8, lwd=3)

plot(out4.gpall50$Inf1 ~ seq(1,1001,1),type='l', col='darkgreen',lwd=3,ylim=c(0,300),#
     ylab="Infected Cattle",xlab="Time",main="50% Wildlife")
lines(out4.gswyn50$Inf1 ~ seq(1,1001,1),lwd=3, col='blue')
legend('topright', legend=c("G. pallidipes","G. swynnertoni"),
       col=c("darkgreen", "blue"), lty=1, cex=0.8, lwd=3)

plot(out4.gpall90$Inf1 ~ seq(1,1001,1),type='l', col='darkgreen',lwd=3,ylim=c(0,300),#
     ylab="Infected Cattle",xlab="Time",main="90% Wildlife")
lines(out4.gswyn90$Inf1 ~ seq(1,1001,1),lwd=3, col='blue')
legend('topright', legend=c("G. pallidipes","G. swynnertoni"),
       col=c("darkgreen", "blue"), lty=1, cex=0.8, lwd=3)

#Comparison of the Tsetse Species: 

par(mfrow=c(1,3))
plot(out4.gpall50$Inf1 - out4.gswyn50$Inf1 ~ seq(1,1001,1), ylim = c(-60,60), type = 'l', lwd=3,
     ylab = "Difference between Tsetse Species", xlab = "Time", col = 'red',main="50% Wildlife Proportion")
abline(h=0, lty = 2, lwd = 2)

plot(out4.gpall10$Inf1 - out4.gswyn10$Inf1 ~ seq(1,1001,1), ylim = c(-60,60), type = 'l', lwd=3,
     ylab = "Difference between Tsetse Species", xlab = "Time", col = 'red',main="10% Wildlife Proportion")
abline(h=0, lty = 2, lwd = 2)

plot(out4.gpall90$Inf1 - out4.gswyn90$Inf1 ~ seq(1,1001,1), ylim = c(-60,60), type = 'l', lwd=3,
     ylab = "Difference between Tsetse Species", xlab = "Time", col = 'red',main="90% Wildlife Proportion")
abline(h=0, lty = 2, lwd = 2)

write.csv(out4.gpall10,"~/University/4th Year/AAT-and-Wildlife/outputs/Gpall10.csv")
write.csv(out4.gpall50,"~/University/4th Year/AAT-and-Wildlife/outputs/Gpall50.csv")
write.csv(out4.gpall90,"~/University/4th Year/AAT-and-Wildlife/outputs/Gpall90.csv")
write.csv(out4.gswyn10,"~/University/4th Year/AAT-and-Wildlife/outputs/Gswyn10.csv")
write.csv(out4.gswyn50,"~/University/4th Year/AAT-and-Wildlife/outputs/Gswyn50.csv")
write.csv(out4.gswyn90,"~/University/4th Year/AAT-and-Wildlife/outputs/Gswyn90.csv")

#Sensitivity Testing of Wildlife Increases at Differnet Wildlife Proportions 
par(mfrow=c(1,3))
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


#Code for Altering the Rplots 
axis(1)
axis(2, at = c(0,50,100,150,200))
Axes = FALSE
