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
