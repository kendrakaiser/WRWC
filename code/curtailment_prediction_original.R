# Original lm prediction method


#0 --------------------
# Apr-Sept diversion & reach gain predictions
#

pred.params.div<-array(NA,c(2,2))
colnames(pred.params.div)<-c("log.vol","sigma")
rownames(pred.params.div)<-c("bw.div", "sc.div")

# # Above Hailey -----
# 
# 
# #plot(var$year[var$year >=2000], var$abv.h[var$year >=2000])
# #hist <- var[var$year >=2000 & var$year < pred.yr,] %>% dplyr::select(abv.h, g.swe, hc.swe, t.cg, t.lw) 
# 
# # linear model 
# div_mod.h<-lm(abv.h~ g.swe+hc.swe+t.cg+t.lw, data=hist) 
# mod_sum[4,1]<-summary(div_mod.h)$adj.r.squared 
# # April 1 Prediction Data 
# pred.dat<- var[var$year == pred.yr,] %>% dplyr::select(g.swe, hc.swe, t.cg, t.lw) 
# # Model output
# preds.div<-predict(div_mod.h,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)
# #preds.params.div[1,1]<-preds.div$fit[1]
# #preds.params.div[1,2]<-preds.div$se.fit
# #preds.params.div.[1,3]<-cor(dat$Total.Div,dat$Reach.Gain)
# 
# png(filename = file.path(fig_dir,"April/Div.abv.Hailey_modelFit.png"),
#     width = 5.5, height = 5.5,units = "in", pointsize = 12,
#     bg = "white", res = 600, type ="cairo-png") 
# 
# fits<-fitted(div_mod.h)
# plot(var$abv.h[var$year >=2000 & var$year < pred.yr],c(fits), xlab="Observed", 
#      ylab="Predicted", xlim=c(5200, 11350), ylim=c(5200, 11350))
# abline(0,1,col="gray50",lty=1)
# dev.off()
# 
# # Above Stanton Crossing the lm does really poorly (0.36) - draw randomly or use a lm that includes natural flow estimate
# png(filename = file.path(fig_dir,"April/Div.abv.Stanton.png"),
#     width = 5.5, height = 5.5,units = "in", pointsize = 12,
#     bg = "white", res = 600, type ="cairo-png") 
# plot(var$year, var$abv.s, xlab="Year", ylab="Diversions (ac-ft)")
# dev.off()
# 
# mod_sum[5,1]<-0.36
# losses between Hailey and Stanton
png(filename = file.path(fig_dir,"April/Losses.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="cairo-png") 
plot(var$year[var$year<2020], var$bws.loss[var$year<2020], xlab="Year", ylab="Annual Losses (ac-ft)")
dev.off()
#plot(var$year[var$year >=2000], var$bws.loss[var$year >=2000])

hist <- var[var$year >=2000 & var$year < 2020,] %>% dplyr::select(bws.loss, g.swe, hc.swe, t.g, t.cg, t.gs) 
# linear model 
bws.loss_mod<-lm(bws.loss~ g.swe+hc.swe+t.cg+ t.gs+t.gs, data=hist) 
mod_sum[6,1]<-summary(bws.loss_mod)$adj.r.squared 
# April 1 Prediction Data 
pred.dat<- var[var$year == pred.yr,] %>% dplyr::select(g.swe,hc.swe,t.cg, t.gs,t.gs) 
# Model output
preds.div<-predict(bws.loss_mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)

png(filename = file.path(fig_dir,"April/Losses_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="cairo-png") 
fits<-fitted(bws.loss_mod)
plot(var$bws.loss[var$year >=2000 & var$year < 2020],c(fits), xlab="Observed", 
     ylab="Predicted", xlim=c(-73000, -39900), ylim=c(-73000, -39900))
abline(0,1,col="gray50",lty=1)
dev.off()

# Total Big Wood Diversions ----
png(filename = file.path(fig_dir,"April/Div.bw.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="cairo-png") 
plot(var$year[var$year<2020], var$div[var$year<2020], xlab="Year", ylab="Big Wood Diversions (ac-ft)")
dev.off()

hist <- var[var$year >=1997 & var$year < 2020,] %>% dplyr::select(div, bws.wq, cg.swe, hc.swe) 
# linear model 
div_mod<-lm(log(var$div[var$year >=1997 & var$year < 2020]) ~ log(cg.swe)+log(hc.swe)+log(bws.wq), data=hist) 
mod_sum[5,1] <- summary(div_mod)$adj.r.squared 
# April 1 Prediction Data 
pred.dat<- var[var$year == pred.yr,] %>% dplyr::select(bws.wq, cg.swe, hc.swe) 
# Model output
preds.div<-predict(div_mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)

pred.params.div[1,1]<-preds.div$fit[1]
pred.params.div[1,2]<-preds.div$se.fit

png(filename = file.path(fig_dir,"April/Diversions_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="cairo-png") 
fits<-fitted(div_mod)
plot(var$div[var$year >=1997 & var$year < 2020],exp(c(fits)), xlab="Observed", 
     ylab="Predicted", xlim=c(32500, 58800), ylim=c(32500, 58800))
abline(0,1,col="gray50",lty=1)
dev.off()

# Silver Creek Diversions ----

png(filename = file.path(fig_dir,"April/Div.sc.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="cairo-png") 
plot(var$year[var$year<2020], var$sc.div[var$year<2020], xlab="Year", ylab="Silver Creek Diversions (ac-ft)")
dev.off()

# g.swe, t.cg, t.gs,t.hc, log.cg, log.lwd
hist <- var[var$year>1993 & var$year < 2020,] %>% dplyr::select(sc.div, g.swe, cg.swe, lwd.swe, t.cg, t.gs, t.hc) 
hist$temps<- rowMeans(cbind(hist$t.cg, hist$t.gs, hist$t.hc), na.rm=TRUE)
# linear model 
sc.div_mod<-lm(log(var$sc.div[var$year>1993 & var$year < 2020]) ~ g.swe+ temps+log(cg.swe)+log(lwd.swe), data=hist) 
mod_sum[6,1] <- summary(sc.div_mod)$adj.r.squared #not gonna work 
# April 1 Prediction Data 
params<- var[var$year == pred.yr,] %>% dplyr::select(g.swe, cg.swe, lwd.swe)
pred.dat<- params %>% slice(rep(1:n(), 5000))
pred.dat$temps<- temp.ran$aj.temps.scd

# Model output
preds.div<-predict(sc.div_mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)

pred.params.div[2,1]<-preds.div$fit[1]
pred.params.div[2,2]<-mean(preds.div$se.fit)

png(filename = file.path(fig_dir,"April/SC_Diversions_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="cairo-png") 
fits<-fitted(sc.div_mod)
plot(var$sc.div[var$year>1993 & var$year < 2020],exp(c(fits)), xlab="Observed", 
     ylab="Predicted", xlim=c(3300, 8200), ylim=c(3300, 8200))
abline(0,1,col="gray50",lty=1)
dev.off()


mod_sum<- round(mod_sum, 3)
png(file.path(fig_dir,"April/model_summary.png"), height = 25*nrow(mod_sum), width = 100*ncol(mod_sum))
grid.table(mod_sum)
dev.off()


# --------------------
# Draw a sample of volumes and water years with similar timing
# These samples are drawn from multivariate normal distributions which are 
# created from the correlation between total volume and center of mass (timing)
# between each gage

# check correlations between flow conditions across the basins
flow.data = var[var$year >= 1997 & var$year < 2020,] %>% dplyr::select(bwb.vol.nat, bwb.cm.nat, bws.vol.nat, bws.cm.nat, cc.vol, cc.cm, sc.vol, sc.cm, div, sc.div) 

# calculate correlations between gages' total volume, diversions and center of mass
cor.mat<-cor(cbind(flow.data[c(1,3,5,7,9,10)],flow.data[c(2,4,6,8)]),use="pairwise.complete")

# create covariance matrix by multiplying by each models standard error
pred.pars<-rbind(pred.params.vol, pred.params.div, pred.params.cm)
outer.prod<-as.matrix(pred.pars[,2])%*%t(as.matrix(pred.pars[,2]))
cov.mat<-cor.mat*outer.prod

# Draw flow volumes using multivariate normal distribution (ac-ft)
vol.pars<-rbind(pred.params.vol, pred.params.div)
vol.sample<-data.frame(mvrnorm(n=5000,mu=(vol.pars[,1]),Sigma=cov.mat[1:6,1:6]))
colnames(vol.sample)<-c("Big Wood Hailey", "Big Wood Stanton","Camas Creek","Silver Creek", "Big Wood Div", "Silver Creek Div")
write.csv(exp(vol.sample), file.path(cd,"April_output/vol.sample.csv"),row.names=F)

# save correlation matrix for model details report
cor.mat.out<-as.data.frame(round(cor.mat,2))
png(file.path(fig_dir,"April/correlation_matrix.png"), height = 25*nrow(cor.mat.out), width = 80*ncol(cor.mat.out))
grid.table(cor.mat.out)
dev.off()
# save output from correlations
write.csv(cov.mat, file.path(cd,"April_output/cov.mat.csv"),row.names=T)
write.csv(pred.pars, file.path(cd,"April_output/pred.pars.csv"),row.names=T)

# Plot boxplots of total annual flow from each model
# Subset for plotting
vol.hist<- as.data.frame(var[var$year < 2020,] %>% dplyr::select(c(bwb.vol.nat, bws.vol.nat, cc.vol)) %>% `colnames<-`(c("Big Wood Hailey Hist", "Big Wood Stanton Hist","Camas Creek Hist")) %>%pivot_longer(everything(),  names_to = "site", values_to = "value") )
vol.hist$value<-vol.hist$value/1000
vol.hist.sm<-as.data.frame(var[var$year < 2020,] %>% dplyr::select(c(sc.vol.nat, sc.div, div)) %>% `colnames<-`(c("Silver Creek Hist", "Silver Creek Div Hist", "Big Wood Div Hist")) %>% pivot_longer(everything(),  names_to = "site", values_to = "value") )
vol.hist.sm$value<-vol.hist.sm$value/1000

vol.pred <-as.data.frame(exp(vol.sample[,1:3])/1000) %>% pivot_longer(everything(),  names_to = "site", values_to = "value")
vol.pred.sm <- as.data.frame(exp(vol.sample[,4:6])/1000) %>% pivot_longer(everything(),  names_to = "site", values_to = "value")

vol.big<- rbind(vol.hist, vol.pred)
vol.sm<- rbind(vol.hist.sm, vol.pred.sm)

vol.big$site<-factor(vol.big$site,levels = c("Big Wood Hailey Hist","Big Wood Hailey", "Big Wood Stanton Hist", "Big Wood Stanton", "Camas Creek Hist", "Camas Creek" ), ordered = TRUE)
vol.sm$site<-factor(vol.sm$site,levels = c("Silver Creek Hist","Silver Creek", "Big Wood Div Hist", "Big Wood Div", "Silver Creek Div Hist", "Silver Creek Div"), ordered = TRUE)

# Plot boxplots of total annual flow from each model
png(filename = file.path(fig_dir_mo,"sampled_volumes.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="cairo-png") 

vol.big %>%
    ggplot(aes(x=site, y=value, fill=site)) +
    geom_boxplot(alpha=0.7) +
    scale_fill_manual(values=c("grey90","blue", "grey90","blue", "grey90","blue")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
    scale_y_continuous(breaks = round(seq(0, max(vol.big$value, na.rm=TRUE), by = 50),1))+
    theme_bw()+
    theme(legend.position="none") +
    ggtitle("Sampled Irrigation Season Volumes") +
    xlab("")+
    ylab("Irrigation Season Volume (KAF)")
dev.off()

png(filename = file.path(fig_dir_mo,"sampled_sc_diversions.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="cairo-png") 

vol.sm %>%
    ggplot(aes(x=site, y=value, fill=site)) +
    geom_boxplot(alpha=0.7) +
    scale_fill_manual(values=c("grey90","blue", "grey90","blue", "grey90","blue")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
    scale_y_continuous(breaks = round(seq(0, max(vol.sm$value, na.rm=TRUE), by = 10),1))+
    theme_bw()+
    theme(legend.position="none") +
    ggtitle("") +
    xlab("")+
    ylab("Irrigation Volume (KAF)")
dev.off()

# CENTER of MASS & Volume Samples

# Draw sample of years with similar center of mass (timing)
cm.data = var[var$year >= 1997 & var$year < 2020,]
cm.data = cm.data %>% dplyr::select(year, bwb.cm.nat, bws.cm.nat,cc.cm, sc.cm) 
cm.data$prob<-NA

# pmvnorm calculates the distribution function of the multivariate normal distribution
for(i in 1:dim(cm.data)[1]){
    vec<-cm.data[i,2:5] # center of mass at each site for a given year
    cm.data$prob[i]<-pmvnorm(lower=as.numeric(vec)-1,
                             upper=as.numeric(vec)+1,mean=pred.params.cm[,1],sigma=cov.mat[6:9,6:9])[1] #need to adjust this to have the upper and lower limits be wider? e.g right now the upper and lowers are only one day off of the original??
}
cm.data$prob<-cm.data$prob/sum(cm.data$prob)
# create normal distribution of years 
CMyear.sample<-sample(cm.data$year,5000,replace=TRUE) 
# Of the 5000 replicates show the percentage that each year represents
cm_sum<-as.data.frame(summary(as.factor(CMyear.sample))/5000)
colnames(cm_sum)<- c("% of sample")
cm_sum<- cm_sum*100
png(file.path(fig_dir,"April/CM_summary.png"), height = 50*nrow(cm_sum), width = 200*ncol(cm_sum))
grid.table(cm_sum)
dev.off()

#save the probabilities for use in the simulation
write.csv(CMyear.sample, file.path(cd,"April_output/CMyear.sample.csv"),row.names=F)

# this version makes the probabilities based on pvnorm distribution, 
# this is not used in the simulations, but is useful for reference
CMyear.sample.prob<-sample(cm.data$year,5000,replace=TRUE, prob=cm.data$prob) 
cm_prob<-as.data.frame(summary(as.factor(CMyear.sample.prob))/5000)
colnames(cm_prob)<- c("% of sample")
cm_prob<- cm_prob*100
png(file.path(fig_dir,"April/CM_summary_prob.png"), height = 50*nrow(cm_prob), width = 200*ncol(cm_prob))
grid.table(cm_prob)
dev.off()


# Draw sample of years with similar volume for comparison
vol.data = var[var$year >= 1997 & var$year < 2020,]
vol.data = vol.data %>% dplyr::select(year, bwb.vol.nat, bws.vol.nat, cc.vol, sc.vol.nat) 
vol.data$prob<-NA

# pmvnorm calculates the distribution function of the multivariate normal distribution
for(i in 1:dim(vol.data)[1]){
    vec<-vol.data[i,2:5]
    vol.data$prob[i]<-pmvnorm(lower=as.numeric(vec)-32000,
                              upper=as.numeric(vec)+32000,mean=exp(pred.params.vol[,1]),corr=cor.mat[1:4,1:4])[1]
}

vol.sample.prob<-sample(vol.data$year,5000,replace=TRUE, prob=vol.data$prob) 
vol_prob<-as.data.frame(summary(as.factor(vol.sample.prob))/5000)*100
colnames(vol_prob)<- c("% of sample")

png(file.path(fig_dir,"April/vol_prob.png"), height = 50*nrow(vol_prob), width = 200*ncol(vol_prob))
grid.table(vol_prob)
dev.off()



# --------------------
# Curtailment predictions
# TODO update data-scraping to go back to 1982, and update the rest of the code accordingly
# TODO test predicting summer ET as a variable for diversions

pred.params.curt<-array(NA,c(9,3)) # 3 columns when comparing to actual shut off date
colnames(pred.params.curt)<-c("adjR2", "pred.date","sigma") # add "act.date" for post-season evaluation
rownames(pred.params.curt)<-c("BW 3/24/1883", "BW 10/14/1884", "BW 6/1/1886", "BL Magic 3/24/1883", "BL Magic 10/14/1884", "BL Magic 6/1/1886", "SC 3/24/1883", "SC 10/14/1884", "SC 6/1/1886")
colnames(vol.sample)<-c("bwb.nat", "bws.nat","cc.vol", "sc.vol", "div", "sc.div")
var$t.curt <- rowMeans(cbind(var$t.sp, var$t.g, var$t.gs, var$t.lw), na.rm=TRUE)

# Big Wood A (3/24/1883)
# div, bwb.vol.nat, t.curt
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="A") %>% subset(subbasin == 'bw_ab_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, div, bwb.vol.nat, t.curt)
# linear model 
bw.a_mod<-lm(shut_off_julian ~ div + bwb.vol.nat + t.curt, data=curt) 
# April 1 Prediction Data 
pred.dat$div <- exp(vol.sample$div)
pred.dat$bwb.vol.nat <- exp(vol.sample$bwb.nat)
pred.dat$t.curt<- temp.ran$aj.temps.curt
# Model output
preds.curt<-predict(bw.a_mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)
pred.params.curt[1,2]<-mean(preds.curt$fit) # mean of predicted
pred.params.curt[1,3]<-mean(preds.curt$se.fit)
pred.params.curt[1,1]<-summary(bw.a_mod)$adj.r.squared 

# Big Wood  B (10/14/1884)
# div, bwb.wq, bwb.vol.nat, t.curt
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="B") %>% subset(subbasin == 'bw_ab_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t.curt, year)
# linear model 
bw.b_mod<-lm(shut_off_julian ~ div +bwb.wq+ bwb.vol.nat + t.curt, data=curt) 
# April 1 Prediction Data 
params<- var[var$year == pred.yr,] %>% dplyr::select(bwb.wq)
pred.dat<- params %>% slice(rep(1:n(), 5000)) # repeat observed data to correspond with predicted data
pred.dat$t.curt<- temp.ran$aj.temps.curt
pred.dat$div <- exp(vol.sample$div)
pred.dat$bwb.vol.nat <- exp(vol.sample$bwb.nat)
# Model output
preds.curt<-predict(bw.b_mod,newdata=pred.dat, se.fit=T,interval="prediction",level=0.95)
pred.params.curt[2,2]<-mean(preds.curt$fit)
pred.params.curt[2,3]<-mean(preds.curt$se.fit)
pred.params.curt[2,1]<-summary(bw.b_mod)$adj.r.squared 

png(filename = file.path(fig_dir,"April/BigWoodB_curtailment.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="cairo-png") 
fits<-fitted(bw.b_mod)
plot(curt$shut_off_julian[curt$year < pred.yr],c(fits), xlab="Observed", 
     ylab="Predicted")
abline(0,1,col="gray50",lty=1)
dev.off()



# Big Wood c (6/1/1886)
# bwb.vol.nat, t.curt
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="C") %>% subset(subbasin == 'bw_ab_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, bwb.vol.nat, t.curt, year)
# linear model 
bw.c_mod<-lm(shut_off_julian ~ bwb.vol.nat + t.curt, data=curt) 
# April 1 Prediction Data 
pred.dat$t.curt<- temp.ran$aj.temps.curt
pred.dat$bwb.vol.nat <- exp(vol.sample$bwb.nat)
# Model output
preds.curt<-predict(bw.c_mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)
pred.params.curt[3,2]<-mean(preds.curt$fit)
pred.params.curt[3,3]<-mean(preds.curt$se.fit)
pred.params.curt[3,1]<-summary(bw.c_mod)$adj.r.squared 

# Below Magic Curtailments ----------
# Big Wood  below magic A (3/24/1883)
# div t
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="A") %>% subset(subbasin == 'bw_bl_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(year, shut_off_julian, div, t.curt)
# linear model 
bwl.a_mod<-lm(shut_off_julian ~ div + t.curt, data=curt) 
# April 1 Prediction Data 
pred.dat$t.curt <- temp.ran$aj.temps.curt
pred.dat$div <- exp(vol.sample$div)
# Model output
preds.curt<-predict(bwl.a_mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)
pred.params.curt[4,2]<-mean(preds.curt$fit)
pred.params.curt[4,3]<-mean(preds.curt$se.fit)
pred.params.curt[4,1]<-summary(bwl.a_mod)$adj.r.squared 

# Big Wood below magic B (10/14/1884)
# bwb.vol.nat t
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="B") %>% subset(subbasin == 'bw_bl_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t.curt)
# linear model 
bwl.b_mod<-lm(shut_off_julian ~bwb.vol.nat + t.curt, data=curt) 
# April 1 Prediction Data 
pred.dat$t.curt<- temp.ran$aj.temps.curt
pred.dat$bwb.vol.nat <- exp(vol.sample$bwb.nat)
# Model output
preds.curt<-predict(bwl.b_mod,newdata=pred.dat, se.fit=T,interval="prediction",level=0.95)
pred.params.curt[5,2]<-mean(preds.curt$fit)
pred.params.curt[5,3]<-mean(preds.curt$se.fit)
pred.params.curt[5,1]<-summary(bwl.b_mod)$adj.r.squared 

# Big Wood  below magic c (6/1/1886)
# bwb.wq bwb.vol.nat t 
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="C") %>% subset(subbasin == 'bw_bl_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, bwb.wq, bwb.vol.nat, t.curt, year)
# linear model 
bwl.c_mod<-lm(shut_off_julian ~ bwb.vol.nat + t.curt, data=curt) 
# April 1 Prediction Data 
params<- var[var$year == pred.yr,]  %>% dplyr::select(bwb.wq)
pred.dat<- params %>% slice(rep(1:n(), 5000)) # repeat observed data to correspond with predicted data
pred.dat$t.curt<- temp.ran$aj.temps.curt
pred.dat$bwb.vol.nat <- exp(vol.sample$bwb.nat)
# Model output
preds.curt<-predict(bwl.c_mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)
pred.params.curt[6,2]<-mean(preds.curt$fit)
pred.params.curt[6,3]<-mean(preds.curt$se.fit)
pred.params.curt[6,1]<-summary(bwl.c_mod)$adj.r.squared 

# Silver Creek Curtailments ----------
# SC A (3/24/1883)
# sc.cm ga.swe, sp.swe, cg.swe, g.swe
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="A") %>% subset(subbasin == 'sc_lw')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, sc.cm, ga.swe, sp.swe, cg.swe, g.swe, year)
# linear model 
sc.a_mod<-lm(shut_off_julian ~ sc.cm + ga.swe+ sp.swe+ cg.swe+ g.swe, data=curt) 
# April 1 Prediction Data 
params<- var[var$year == pred.yr,] %>% dplyr::select(ga.swe, sp.swe, cg.swe, g.swe)
pred.dat<- params %>% slice(rep(1:n(), 5000)) # repeat observed data to correspond with predicted data
pred.dat$sc.cm <- sample(cm.data$sc.cm,5000,replace=TRUE) # normal distribution of center of mass
# Model output
preds.curt<-predict(sc.a_mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)
pred.params.curt[7,2]<-mean(preds.curt$fit)
pred.params.curt[7,3]<-mean(preds.curt$se.fit)
pred.params.curt[7,1]<-summary(sc.a_mod)$adj.r.squared 

# SC B (10/14/1884)
# sc.div, ga.swe 
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="B") %>% subset(subbasin == 'sc_lw')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, sc.div, ga.swe, year)
# linear model 
sc.b_mod<-lm(shut_off_julian ~ sc.div + ga.swe, data=curt) 
# April 1 Prediction Data 
params<- var[var$year == pred.yr,]  %>% dplyr::select(ga.swe)
pred.dat<- params %>% slice(rep(1:n(), 5000)) # repeat observed data to correspond with predicted data
pred.dat$sc.div <- exp(vol.sample$sc.div)
# Model output
preds.curt<-predict(sc.b_mod,newdata=pred.dat, se.fit=T,interval="prediction",level=0.95)
pred.params.curt[8,2]<-mean(preds.curt$fit)
pred.params.curt[8,3]<-mean(preds.curt$se.fit)
pred.params.curt[8,1]<-summary(sc.b_mod)$adj.r.squared 

# SC C (6/1/1886)
# sc.div, ga.swe, cg.swe, lwd.swe, t
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="C") %>% subset(subbasin == 'sc_lw')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, sc.div, ga.swe, cg.swe, lwd.swe, t.curt, year)
# linear model 
sc.c_mod<-lm(shut_off_julian ~ sc.div+ ga.swe+ cg.swe+ lwd.swe + t.curt, data=curt) 
# April 1 Prediction Data 
params<- var[var$year == pred.yr,]  %>% dplyr::select(ga.swe, cg.swe, lwd.swe)
pred.dat<- params %>% slice(rep(1:n(), 5000)) # repeat observed data to correspond with predicted data
pred.dat$t.curt<- temp.ran$aj.temps.curt
pred.dat$sc.div <- exp(vol.sample$sc.div)
# Model output
preds.curt<-predict(sc.c_mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)
pred.params.curt[9,2]<-mean(preds.curt$fit)
pred.params.curt[9,3]<-mean(preds.curt$se.fit)
pred.params.curt[9,1]<-summary(sc.c_mod)$adj.r.squared 



# Curtailment Summary ------
julian_curt<- data.frame(matrix(ncol = 10, nrow = length(min(curtailments$year):max(curtailments$year))))
colnames(julian_curt)<- c("year", "uwr_a", "uwr_b", "uwr_c", "lwr_a", "lwr_b", "lwr_c", "sc_a", "sc_b", "sc_c")
julian_curt$year <- min(curtailments$year):max(curtailments$year)
julian_curt$uwr_a <- curtailments %>% subset(water_right_cat =="A") %>% subset(subbasin == 'bw_ab_magic') %>% dplyr::select(shut_off_julian) 
julian_curt$uwr_b <- curtailments %>% subset(water_right_cat =="B") %>% subset(subbasin == 'bw_ab_magic') %>% dplyr::select(shut_off_julian) 
julian_curt$uwr_c <- curtailments %>% subset(water_right_cat =="C") %>% subset(subbasin == 'bw_ab_magic') %>% dplyr::select(shut_off_julian) 
julian_curt$lwr_a <- curtailments %>% subset(water_right_cat =="A") %>% subset(subbasin == 'bw_bl_magic') %>% dplyr::select(shut_off_julian) 
julian_curt$lwr_b <- curtailments %>% subset(water_right_cat =="B") %>% subset(subbasin == 'bw_bl_magic') %>% dplyr::select(shut_off_julian) 
julian_curt$lwr_c <- curtailments %>% subset(water_right_cat =="C") %>% subset(subbasin == 'bw_bl_magic') %>% dplyr::select(shut_off_julian) 
julian_curt$sc_a <- curtailments %>% subset(water_right_cat =="A") %>% subset(subbasin == 'sc_lw') %>% dplyr::select(shut_off_julian) 
julian_curt$sc_b <- curtailments %>% subset(water_right_cat =="B") %>% subset(subbasin == 'sc_lw') %>% dplyr::select(shut_off_julian) 
julian_curt$sc_c <- curtailments %>% subset(water_right_cat =="C") %>% subset(subbasin == 'sc_lw') %>% dplyr::select(shut_off_julian) 
#colnames(julian_curt)<- c("year", "uwr_a", "uwr_b", "uwr_c", "lwr_a", "lwr_b", "lwr_c", "sc_a", "sc_b", "sc_c")

# calculate correlations between cutoff dates, diversions and center of mass
new_jul<-julian_curt[julian_curt$year >= 1997,]
all.cor.mat<-cor(cbind(flow.data[c(1,3,5,7,9,10)],flow.data[c(2,4,6,8)], new_jul[-1]),use="pairwise.complete")
curt.cor.mat<-cor(julian_curt[-1], use="pairwise.complete")

# create covariance matrix by multiplying by each models standard error
curt.outer.prod<-as.matrix(pred.params.curt[,3])%*%t(as.matrix(pred.params.curt[,3]))
curt.cov.mat<-curt.cor.mat*curt.outer.prod
# Draw curtailment dates using multivariate normal distribution
curt.sample<-data.frame(mvrnorm(n=5000,mu=(pred.params.curt[,2]),Sigma=curt.cov.mat))

colnames(curt.sample)<-c("bw_a", "bw_b", "bw_c", "lwbw_a", "lwbw_b", "lwbw_c", "sc_a", "sc_b", "sc_c")
write.csv(curt.sample, file.path(cd,"April_output/curt.sample.csv"),row.names=F)

# Plot boxplots of predicted curtailment dates from each model -> modelOutput.Rmd
png(filename = file.path(fig_dir,"April/sampled_curtailments.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="cairo-png") 

curt.sample %>% pivot_longer(everything(),  names_to = "site", values_to = "value") %>%
    ggplot(aes(x=site, y=as.Date(value, origin=as.Date(paste(pred.yr,"-01-01",sep=""))), fill=site)) +
    theme_bw()+
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    scale_y_date(date_breaks = "1 week", date_labels = "%b %d")+
    theme(legend.position="none") +
    ggtitle("Sampled Curtailment Dates") +
    xlab("")+
    ylab("Curtailment Date")
dev.off()


# Change julian to actual date for readibility
pred.params.curt<- as.data.frame(pred.params.curt) %>% round(2)
pred.params.curt$pred.date<-as.Date(pred.params.curt$pred.date, origin=as.Date(paste(pred.yr,"-01-01",sep="")), format='%m/%d')
write.csv(pred.params.curt, file.path(cd,"April_output/pred.curtailments.csv"))
#pred.params.curt$act.date[1]<- curtailments$shut_off_date[curtailments$water_right_cat == "A" & curtailments$subbasin == 'bw_ab_magic' & curtailments$year == pred.yr]
#pred.params.curt$act.date[2]<- curtailments$shut_off_date[curtailments$water_right_cat == "B" & curtailments$subbasin == 'bw_ab_magic' & curtailments$year == pred.yr]
#pred.params.curt$act.date[3]<- curtailments$shut_off_date[curtailments$water_right_cat == "C" & curtailments$subbasin == 'bw_ab_magic' & curtailments$year == pred.yr]
#pred.params.curt$act.date[4]<- curtailments$shut_off_date[curtailments$water_right_cat == "A" & curtailments$subbasin == 'bw_bl_magic' & curtailments$year == pred.yr]
#pred.params.curt$act.date[5]<- curtailments$shut_off_date[curtailments$water_right_cat == "B" & curtailments$subbasin == 'bw_bl_magic' & curtailments$year == pred.yr]
#pred.params.curt$act.date[6]<- curtailments$shut_off_date[curtailments$water_right_cat == "C" & curtailments$subbasin == 'bw_bl_magic' & curtailments$year == pred.yr]
#pred.params.curt$act.date[7]<- curtailments$shut_off_date[curtailments$water_right_cat == "A" & curtailments$subbasin == 'sc_lw' & curtailments$year == pred.yr]
#pred.params.curt$act.date[8]<- curtailments$shut_off_date[curtailments$water_right_cat == "B" & curtailments$subbasin == 'sc_lw' & curtailments$year == pred.yr]
#pred.params.curt$act.date[9]<- curtailments$shut_off_date[curtailments$water_right_cat == "C" & curtailments$subbasin == 'sc_lw' & curtailments$year == pred.yr]


png(file.path(fig_dir,"April/curt_summary.png"), height = 50*nrow(pred.params.curt), width = 200*ncol(pred.params.curt))
grid.table(pred.params.curt)
dev.off()
