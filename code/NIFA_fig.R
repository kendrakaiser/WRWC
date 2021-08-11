plot(var$year[complete.cases(hist)], hist$sc.vol.nat[complete.cases(hist)]/100, pch=20, ylim=c(290,750), xlab="Year", ylab="Streamflow Volume (100 ac-ft)")
points(var$year[complete.cases(hist)], c(fits)/100, pch=20, col="red", ylim=c(85,750))
segments(var$year[complete.cases(hist)], hist$sc.vol.nat[complete.cases(hist)]/100, x1 = var$year[complete.cases(hist)], y1 = c(fits)/100)
#points(var$year[var$year < 2020], hist$sc.wq[complete.cases(hist)], pch=20, col="blue", ylim=c(85,750))

legend("right", 
       legend = c("Observed Summer Streamflow", "Modeled Summer Streamflow"), 
       col = c("black", "red"), 
       pch = c(20,20,20), 
       cex = 0.8,
       inset=c(-.5,0),
       horiz = F)
