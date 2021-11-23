
# Local File Path
cd <<- '~/Desktop/WRWC'
data_dir <<- file.path(cd, 'April_output') 
sim <- read.csv(file.path(data_dir, 'BWS.nat.sim.csv'))
dates<-seq(as.Date(paste(pred.yr,"-04-01",sep="")),as.Date(paste(pred.yr,"-09-30",sep="")),"day")

# Calculate prediction Intervals
numpreds<- 24 # nat+reg + div *4
pi<-data.frame(array(NA,c(183,numpreds)))
meanQ<-data.frame(array(NA,c(183,4)))
medQ<-data.frame(array(NA,c(183,4)))

pred.int<-function(location){
    lo<-apply(location,1,quantile,0.05, na.rm=TRUE)
    hi<-apply(location,1,quantile,0.95, na.rm=TRUE)
    meanQ<-apply(location,1,mean, na.rm=TRUE)
    medQ<-apply(location,1,median, na.rm=TRUE)
    
    return(cbind(lo, hi, meanQ, medQ))
}

out<- as.data.frame(pred.int(sim[,-1]))
# Big Wood @ Stanton
#png(filename = file.path(mo_fig_dir, "BWS_Simulation.png"),
 #   width = 5.5, height = 5.5,units = "in", pointsize = 12,
  #  bg = "white", res = 600) 
plot(dates, out[,3], type="n", xlab="Date", ylab ="Flow (cfs)",
     main = "Big Wood Streamflow at Stanton Crossing", ylim=c(min(out[,1]), max(out[,2])))
polygon(c(dates[1], dates, rev(dates)), c(out[1,1], out[,2], rev(out[,1])), 
        col = "gray90", border = NA)
#lines(dates,data$meanQ[196:378],lwd=1.5,col="black")
lines(dates,out[,3],lwd=2.5,col="blue")
lines(dates,out[,4],lwd=2.5,col="orange")
legend("topright", inset=.02, legend=c("Historic Avg.", "Avg Simulation"),
       col=c("black", "blue"), lty=1:1, lwd=1:2.5,cex=0.8, box.lty=0)


mean_cuml<-cumsum(out[,3])
med_cuml<-cumsum(out[,4])

totAF<- rep(total_af, length(dates))

plot(dates, out[,3], type="n", xlab="Date", ylab ="Cumulative Flow (cfs)",
     main = "Big Wood Cumulative \nStreamflow at Stanton Crossing", ylim=c(min(out[,1]), max(mean_cuml)))
lines(dates,mean_cuml,lwd=2.5,col="blue")
lines(dates,med_cuml,lwd=2.5,col="orange")
lines(dates,totAF*0.504,lwd=1.5,col="grey")


