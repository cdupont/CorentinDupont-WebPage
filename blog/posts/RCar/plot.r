require(stats)
D = 150
T = 10
t = seq(0, 80, 0.01)
x = -D*exp(-(t/T))+D
v = (D/T)*exp(-(t/T))
png("CarPos.png")
plot(t, x, type="l", main="Evolution of position through time", xlab="time (s)", ylab="position (m)", xlim=c(0,80), ylim=c(0, D+10),  xaxs = "i", yaxs = "i")
abline(h=D, col="red")
text(2, D-3, "D", 1, col="red")

png("CarSpeed.png")
plot(t, v, type="l", main="Evolution of speed through time", xlab="time (s)", ylab="speed (m/s)", xlim=c(0,80), ylim=c(0,(D+10)/T), xaxs = "i", yaxs = "i")
text(5, D/T, "D/T", 1, col="red")
abline(h=1, col="red")



