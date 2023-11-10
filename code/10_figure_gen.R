# Figures for data analysis and model output from wood river streamflow florecasting


### Streamflow Simulation Figures
#-------------------------------------------------------------------------------

### Average Annual streamflow with maximum and minimum daily flows
streamflow_data$Date <- as.Date(streamflow_data$datetime)
streamflow_data$doy <- yday(streamflow_data$Date)

#Calculate day of water-year
water_year_begin <- ymd('1987-10-01')-1
#deal with leap years
streamflow_data$doWY<- ((streamflow_data$doy - yday(water_year_begin)) %% ifelse(leap_year(year(streamflow_data$Date)), 366, 365)) +1


data <- streamflow_data %>% filter(abv == 'bwh') %>% group_by(doWY) %>% dplyr::mutate(meanQ=mean(value, na.rm=TRUE))

minBW=min(pi[,1], pi[,5],data$meanQ[457:639])
maxBW=max(pi[,2], pi[,6],data$meanQ[457:639])
# Big Wood @ Hailey
png(filename = file.path(fig_dir_mo, "bwh_Simulation.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(dates, pi[,3], type="n", xlab="Date", ylab ="Flow (cfs)",
     main = "Big Wood River Streamflow at Hailey", ylim=c(minBW, maxBW))
polygon(c(dates[1], dates, rev(dates)), c(pi[1,1], pi[,2], rev(pi[,1])), 
        col = "gray90", border = NA)
lines(dates,data$meanQ[457:639],lwd=1.5,col="black")
lines(dates,pi[,3],lwd=2.5,col="blue")
legend("topright", inset=.02, legend=c("Historic Avg.", "Avg Simulation"),
       col=c("black", "blue"), lty=1:1, lwd=1:2.5,cex=0.8, box.lty=0)
#flow= bwh.wy[bwh.wy$wy == pred.yr, "bwh.nat.q"]
#lines(dates,flow[183:365], lwd=2, col="green")
dev.off()

# Big Wood @ Stanton
data <- streamflow_data %>% filter(abv == 'bws') %>% group_by(doWY) %>% dplyr::mutate(meanQ=mean(value, na.rm=TRUE))
# Big Wood @ Stanton
png(filename = file.path(fig_dir_mo, "BWS_Simulation.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(dates, pi[,7], type="n", xlab="Date", ylab ="Flow (cfs)",
     main = "Big Wood Streamflow at Stanton Crossing", ylim=c(minBW, maxBW))
polygon(c(dates[1], dates, rev(dates)), c(pi[1,5], pi[,6], rev(pi[,5])), 
        col = "gray90", border = NA)
lines(dates,data$meanQ[196:378],lwd=1.5,col="black")
lines(dates,pi[,7],lwd=2.5,col="blue")
legend("topright", inset=.02, legend=c("Historic Avg.", "Avg Simulation"),
       col=c("black", "blue"), lty=1:1, lwd=1:2.5,cex=0.8, box.lty=0)
#flow= bws.wy[bws.wy$wy == pred.yr, "bws.nat.q"]
#lines(dates,flow[183:365], lwd=2, col="green")
dev.off()

# Silver Creek
data <- streamflow_data %>% filter(abv == 'sc') %>% group_by(doWY) %>% dplyr::mutate(meanQ=mean(value, na.rm=TRUE))

# Silver Creek
png(filename = file.path(fig_dir_mo, "SC_Simulation.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(dates, pi[,11], type="n", xlab="Date", ylab ="Flow (cfs)",
     main = "Silver Creek Streamflow Simulation", ylim=c(min(pi[,9]), max(pi[,10])))
polygon(c(dates[1], dates, rev(dates) ), c(pi[1,9], pi[,10], rev(pi[,9])), 
        col = "gray90", border = NA)
lines(dates,data$meanQ[91:273],lwd=1.5,col="black")
lines(dates,pi[,11],lwd=2.5,col="blue")
legend("topright", inset=.02, legend=c("Historic Avg.", "Avg Simulation"),
       col=c("black", "blue"), lty=1:1, lwd=1:2.5,cex=0.8, box.lty=0)
#flow= sc.wy[sc.wy$wy == pred.yr, "sc.nat"]
#lines(dates,flow[183:365], lwd=2, col="green")
dev.off()

# Camas Creek

data <- streamflow_data %>% filter(abv == 'cc') %>% group_by(doWY) %>% dplyr::mutate(meanQ=mean(value, na.rm=TRUE))
mincc=min(pi[,13], data$meanQ[91:273])
maxcc=max(pi[,14], data$meanQ[91:273])
# Camas Creek
png(filename = file.path(fig_dir_mo, "CC_Simulation.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(dates,pi[,15], type="n", xlab="Date", ylab ="Flow (cfs)",
     main = "Camas Creek Streamflow Simulation", ylim=c(mincc, maxcc))
polygon(c(dates[1], dates, rev(dates) ), c(pi[1,13], pi[,14], rev(pi[,13])), 
        col = "gray90", border = NA)
lines(dates,data$meanQ[91:273],lwd=1.5,col="black")
lines(dates,pi[,15],lwd=2.5,col="blue")
legend("topright", inset=.02, legend=c("Historic Avg.", "Avg Simulation"),
       col=c("black", "blue"), lty=1:1, lwd=1:2.5,cex=0.8, box.lty=0)
#flow= cc.wy[cc.wy$wy == pred.yr, "Flow"]
#lines(dates,flow[183:365], lwd=2, col="green")
dev.off()


#Multiple Water Years plotted on one figure
plot(dates, bwh.wy$value[bwh.wy$wy == 2006][183:365], xlab="Date", ylab ="Flow (cfs)", type='l', col="black", ylim=c(0,6650))
lines(dates,bwh.wy$value[bwh.wy$wy == 2014][183:365], lwd=1, col="black")
lines(dates,bwh.wy$value[bwh.wy$wy == 2013][183:365], lwd=1, col="black")
lines(dates,bwh.wy$value[bwh.wy$wy == 2019][183:365], lwd=1, col="blue")
lines(dates,bwh.wy$value[bwh.wy$wy == 1998][183:365], lwd=1, col="red")
