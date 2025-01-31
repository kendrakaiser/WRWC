# import and analyze water right data in WD37
# July 13th, 2022
# Kendra Kaiser
# ---------------------------------
library(dplyr)
library(viridis)
library(ggnewscale)

# #import data
# wr_og<-read.csv(file.path(input_dir,'WD37_Irrigation rights Big Wood Above Magic.csv'))
# 
# #subset to relevant columns
# wr<- wr_og %>% dplyr::select('Right.ID', 'Priority.Date', 'PD.Sort.Key', "Overall.Max.Diversion.Rate.cfs.",
#                       "Overall.Max.Diversion.Volume.af.") %>% mutate(priority.date=
#                       as.Date(Priority.Date, format ="%m/%d/%Y"))
# 
# #set any water rights without a cfs limit to zero
# wr$Overall.Max.Diversion.Rate.cfs.[is.na(wr$Overall.Max.Diversion.Rate.cfs.)] <- 0
# 
# # subset to the single water right ID for a given wr and arrange by priority date, senior to junior
# wr.sub<-  wr %>% filter(!duplicated(wr$Right.ID)) %>% arrange(priority.date)
# 
# # sum the cfs of the water rights
# wr.sub$cuml.cfs <- cumsum(wr.sub$Overall.Max.Diversion.Rate.cfs.)
# 
# dbWriteTable(conn,"waterrights",wr.sub)

# # Plot cumulative water rights
# ggplot(wr.sub, aes(priority.date, cuml.cfs))+
#   geom_point()+
#   theme_bw()+
#   xlab("Priority Date")+
#   ylab("Cumulative Water Rights in the Basin (cfs)")

# ------------------------------------------------------------------------------
# Identify when curtailments will occur based on mean simulated streamflow
# ------------------------------------------------------------------------------

# Big Wood Water Rights
wr.sub=dbReadTable(conn,"waterrights")

# big wood hailey mean simulated flow
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

#TODO: clean this up to shorten this 
wr.cutoffs$cut.wr.hi<-Reduce(c, lapply(wr.cutoffs$bwh.hiQ, cutoff, wr.sub))
#wr.cutoffs$cut.wr.hi[1:which(wr.cutoffs$bwh.hiQ == max(wr.cutoffs$bwh.hiQ))]<-NA #ignore cutoffs before peak
wr.cutoffs$cut.wr.med<-Reduce(c, lapply(wr.cutoffs$bwh.medQ, cutoff, wr.sub))
wr.cutoffs$cut.wr.low<-Reduce(c, lapply(wr.cutoffs$bwh.lowQ, cutoff, wr.sub))

dbWriteTable(conn,"wr.cutoffs", wr.cutoffs, overwrite=TRUE)

