# SAMPLE MODEL COMPARISON PLOTS
#
# SPK 03-MAR-2021
#

par(mfrow=c(1,2))
plot(out2$Inf1 ~ seq(1,1001,1), type = 'l', col = 'darkgreen', lwd = 3, ylim = c(0,200),
     ylab = "Infected Cattle", xlab = "Time")
lines(out4.gswyn$Inf1 ~ seq(1,1001,1), lwd = 3, col = 'blue')
legend('topright', legend=c("2 Species", "4 Species"),
       col=c("darkgreen", "blue"), lty=1, cex=0.8, lwd = 3)



plot(out2$Inf1-out4.gswyn$Inf1 ~ seq(1,1001,1), ylim = c(-60,60), type = 'l', lwd = 3,
     ylab = "Difference between 2 Sp Model & 4 Sp Model", xlab = "Time", col = 'red')
abline(h=0, lty = 2, lwd = 2)

