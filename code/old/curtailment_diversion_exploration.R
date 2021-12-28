# -------------------------------------------------------------
# Diversions above Hailey
# -------------------------------------------------------------
hist <- var[var$year >= 1997 & var$year < pred.yr,] %>% dplyr::select(bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw, year) 
hist$log.wq <- log(hist$bwb.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
#use regsubsets to plot the results
regsubsets.out<-regsubsets(var$abv.h[var$year >= 1997 & var$year < pred.yr]~., data=hist, nbest=3, nvmax=8)
# cg.swe, g.swe, hc.swe, t.cg, t.g, t.gs, t.lw, log.lwd, r2=.65 post 1997, no trend with time

# Diversions above Stanton Crossing
plot(var$year, var$abv.s)
hist <- var[var$year >= 1997 & var$year < pred.yr,] %>% dplyr::select(bwb.wq, bws.wq, bwb.vol.nat, bws.vol.nat, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw, year) 
regsubsets.out<-regsubsets(var$abv.s[var$year >= 1997 & var$year < pred.yr]~., data=hist, nbest=3, nvmax=8)
# these all do really poorly, and no trends in the data at all, so pull randomly

# Losses Between Hailey and Stanton Crossing
plot(var$year, var$bws.loss)
plot(var$year[var$year >=2000], var$bws.loss[var$year >=2000])
hist <- var[var$year >= 2000 & var$year < pred.yr,] %>% dplyr::select(bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw, year) 
#use regsubsets to plot the results
regsubsets.out<-regsubsets(var$bws.loss[var$year >= 2000 & var$year < pred.yr]~., data=hist, nbest=2, nvmax=8)
# t.cg, t.gs, lowest BIC, R2 == 0.55
# g.swe, hc.swe, t.g, t.cg, t.gs highest R2 and third lowest BIC

# Total Diversions
hist <- var[var$year >= 1997 & var$year < 2020,] %>% dplyr::select(bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw, year) 
hist$log.wq <- log(hist$bwb.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
hist<-hist %>% dplyr::select(-cg.swe, -hc.swe, -bwb.wq)
regsubsets.out<-regsubsets(var$div[var$year >= 1997 & var$year < 2020]~., data=hist, nbest=3, nvmax=8)

# Feb 1 Max R2 of 0.13 -- wont work
# Mar 1 log gs r2 0.27

# Silver Creek Diversions
hist <- var[var$year < 2020,] %>% dplyr::select(sc.wq, bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw, t.f, year) 
hist$log.scwq <- log(hist$sc.wq)
hist$log.wq <- log(hist$bwb.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)

regsubsets.out<-regsubsets(log(var$sc.div[var$year < 2020])~., data=hist, nbest=3, nvmax=8)

regsubets.res<-cbind(regsubsets.out$size,regsubsets.out$adjr2, regsubsets.out$bic)
png(filename = file.path(fig_dir, "Regsubsets_example.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
#quartz(title="BIC",10,10)
plot(regsubsets.out, scale = "bic", main="BIC For the best model of a given size")
dev.off()

quartz(title="Adjusted R^2",10,10)
plot(regsubsets.out, scale = "adjr2", main="Adjusted R^2 For the best model of a given size")

rs<-summary(regsubsets.out)
quartz(title="BIC v R2",10,10)
plot(rs$bic, rs$adjr2, xlab="BIC", ylab="adj R2")

# ----------------------------------------------
# Curtailments
curtailments = read.csv(file.path(cd,'historic_shutoff_dates_071520.csv'))
#bigwood A
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="A") %>% subset(subbasin == 'bw_ab_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t)
# div, bwb.vol.nat, t (0.67)

#bigwood B
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="B") %>% subset(subbasin == 'bw_ab_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t)
# div, bwb.wq, bwb.vol.nat, t (0.89)

#bigwood c
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="C") %>% subset(subbasin == 'bw_ab_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t)
#  bwb.vol.nat, t (0.87)

# bigwood bl magic A
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="A") %>% subset(subbasin == 'bw_bl_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t)
# bwb.wq bwb.vol.nat t.g t.lw (0.35)
# div t (0.300)

#bigwood bl magic B
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="B") %>% subset(subbasin == 'bw_bl_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t)
# bwb.vol.nat t (0.44)

#bigwood bl magic c
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="C") %>% subset(subbasin == 'bw_bl_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t)
# bwb.wq bwb.vol.nat t (0.47)

# sc A
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="A") %>% subset(subbasin == 'sc_lw')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, sc.div, sc.cm, sc.wq, ga.swe, sp.swe, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(-c(t.sp, t.g, t.gs, t.lw))
# sc.cm ga.swe, sp.swe, cg.swe, g.swe (0.29)

# sc B
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="B") %>% subset(subbasin == 'sc_lw')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, sc.div, sc.cm, sc.wq, ga.swe, sp.swe, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(-c(t.sp, t.g, t.gs, t.lw))
# sc.div, ga.swe (0.68)

# sc c
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="C") %>% subset(subbasin == 'sc_lw')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, sc.div, sc.cm, sc.wq, ga.swe, sp.swe, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(-c(t.sp, t.g, t.gs, t.lw))
# sc.div, ga.swe, cg.swe, lwd.swe, t (0.83)


#use regsubsets to plot the results
regsubsets.out<-regsubsets(shut_off_julian~., data=curt, nbest=3, nvmax=5)
regsubets.res<-cbind(regsubsets.out$size,regsubsets.out$adjr2, regsubsets.out$bic)
quartz(title="Adjusted R^2",10,10)
plot(regsubsets.out, scale = "adjr2", main="Adjusted R^2 For the best model of a given size")
quartz(title="BIC",10,10)
plot(regsubsets.out, scale = "bic", main="BIC For the best model of a given size")
rs<-summary(regsubsets.out)
quartz(title="R2 v BIC",10,10)
plot(rs$bic, rs$adjr2, xlab="BIC", ylab="adj R2")