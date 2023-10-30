# import and analyze water right data in WD37
# July 13th, 2022
# Kendra Kaiser
# ---------------------------------

#import data
wr_og<-read.csv(file.path(data_dir,'WD37__01192021 Irrigation rights Big Wood Above Magic.csv'))

#subset to relevant colums
wr<- wr_og %>% select('Right.ID', 'Priority.Date', 'PD.Sort.Key', "Overall.Max.Diversion.Rate.cfs.", 
                      "Overall.Max.Diversion.Volume.af.") %>% mutate(priority.date=
                      as.Date(Priority.Date, format ="%m/%d/%Y")) 

#set any water rights without a cfs limit to zero
wr$Overall.Max.Diversion.Rate.cfs.[is.na(wr$Overall.Max.Diversion.Rate.cfs.)] <- 0

# subset to the single water right ID for a given wr and arrange by priority date, senior to junior
wr.sub<-  wr %>% filter(!duplicated(wr$Right.ID)) %>% arrange(priority.date)

# sum the cfs of the water rights
wr.sub$cuml.cfs <- cumsum(wr.sub$Overall.Max.Diversion.Rate.cfs.)

# Plot cumulative water rights 
ggplot(wr.sub, aes(priority.date, cuml.cfs))+
  geom_point()+
  theme_bw()+
  xlab("Priority Date")+
  ylab("Cumulative Water Rights in the Basin (cfs)")

# ------------------------------------------------------------------------------
# Identify when curtailments will occur based on mean simulated streamflow
# ------------------------------------------------------------------------------

# Big Wood Water Rights -- should this be using flow at stanton, 
# or cumulative flow going into Magic?

# big wood stanton mean simulated flow 
wr.cutoffs<- data.frame("date" = array(NA,c(183)))
wr.cutoffs$date<- dates
wr.cutoffs$doy<- yday(dates)

wr.cutoffs$bwh.meanQ<-pi[,"bwh.mean"]
wr.cutoffs$bwh.medQ<-pi[,"bwh.med"]
wr.cutoffs$bwh.hiQ<-pi[,"bwh.hi"]
wr.cutoffs$bwh.lowQ<-pi[,"bwh.low"]

# find the date of the most senior wr that is above the "current" flow
cutoff<- function(flow, wr_array){
  cut.wr<- wr_array$priority.date[min(which(wr_array$cuml.cfs > flow))]
  return(cut.wr)
}

#clean this up to shorten this 
wr.cutoffs$cut.wr.hi<-Reduce(c, lapply(wr.cutoffs$bwh.hiQ, cutoff, wr.sub))
#wr.cutoffs$cut.wr.hi[1:which(wr.cutoffs$bwh.hiQ == max(wr.cutoffs$bwh.hiQ))]<-NA #ignore cutoffs before peak

wr.cutoffs$cut.wr.med<-Reduce(c, lapply(wr.cutoffs$bwh.medQ, cutoff, wr.sub))
#wr.cutoffs$cut.wr.med[1:which(wr.cutoffs$bwh.medQ == max(wr.cutoffs$bwh.medQ))]<-NA #ignore cutoffs before peak

wr.cutoffs$cut.wr.low<-Reduce(c, lapply(wr.cutoffs$bwh.lowQ, cutoff, wr.sub))
#wr.cutoffs$cut.wr.low[1:which(wr.cutoffs$bwh.lowQ == max(wr.cutoffs$bwh.lowQ))]<-NA #ignore cutoffs before peak

wr.cutoffs$ymo.hi<- zoo::as.yearmon(wr.cutoffs$cut.wr.hi)
wr.cutoffs$ymo.med<- zoo::as.yearmon(wr.cutoffs$cut.wr.med)
wr.cutoffs$ymo.low<- zoo::as.yearmon(wr.cutoffs$cut.wr.low)

ymo.list<-unique(c(wr.cutoffs$ymo.hi,wr.cutoffs$ymo.med,wr.cutoffs$ymo.low))
ymo.list<- ymo.list[order(ymo.list)]



wr.ts<- function(wr.co, flow, sitename="name"){
  wr.co[sitename]<- flow
  wr.co$cut.wr<-Reduce(c, lapply(wr.co[sitename], cutoff, wr.sub))
  wr.co$cut.wr[1:which(wr.co[sitename] == max(wr.co[sitename]))]<-NA #ignore cutoffs before peak
  wr.co$ymo<- zoo::as.yearmon(wr.co$cut.wr)
  wr.co- wr.co[complete.cases(wr.co),]
  
  return(wr.co)
}
#wr.cutoffs<- wr.ts(wr.cutoffs, pi[,"bwh.med"], sitename = "bws.medQ")


ggplot(data=wr.cutoffs) + 
  geom_point(aes(x=date, y=bwh.hiQ, color= as.factor(ymo.hi)), show.legend = FALSE)+
  scale_color_viridis(discrete = TRUE, option = "turbo", direction=-1, limits = as.factor(ymo.list))+ 
  new_scale_colour() +
  geom_point(aes(x=date, y=bwh.medQ, color= as.factor(ymo.med)), show.legend = FALSE)+
  scale_color_viridis(discrete = TRUE, option = "turbo", direction=-1, limits = as.factor(ymo.list))+  
  new_scale_colour() +
  geom_point(aes(x=date, y=bwh.lowQ, color= as.factor(ymo.low)))+
  scale_color_viridis(discrete = TRUE, option = "turbo", direction=-1, limits = as.factor(ymo.list))+  
  theme_bw()+
  scale_y_continuous(n.breaks=8)+
  scale_x_date(date_breaks = "weeks" , date_labels = "%m/%d", limits=c(wr.cutoffs$date[1], wr.cutoffs$date[183]))+
  labs(color = "Surface Water Rights")+
  ylab("Average Simulated Streamflow @ Hailey (cfs)")+
  xlab("Date")

ggsave(filename = file.path(fig_dir_mo, "curtailment_recession.png"),
       width = 14.5, height = 5.5, units = "in", pointsize = 12,
       bg = "white") 

#the big problem / question here is that the old water rights shouldnt get turned off, so I'm wondering which data should be used here??
# Silver Creek 

