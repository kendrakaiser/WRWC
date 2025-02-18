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
  cut.wr<- wr_array$priority.date[min(which(wr_array$cuml.cfs > flow))] %>% filter()
  
  return(cut.wr)
}

#TODO: clean this up to shorten this 
wr.cutoffs$cut.wr.hi<-Reduce(c, lapply(wr.cutoffs$bwh.hiQ, cutoff, wr.sub))
#wr.cutoffs$cut.wr.hi[1:which(wr.cutoffs$bwh.hiQ == max(wr.cutoffs$bwh.hiQ))]<-NA #ignore cutoffs before peak
wr.cutoffs$cut.wr.med<-Reduce(c, lapply(wr.cutoffs$bwh.medQ, cutoff, wr.sub))
wr.cutoffs$cut.wr.low<-Reduce(c, lapply(wr.cutoffs$bwh.lowQ, cutoff, wr.sub))

dbWriteTable(conn,"wr.cutoffs", wr.cutoffs, overwrite=TRUE)


#run historic hydrographs through the curtailment model to assess how well it works
wr_obs<-read.csv(file.path(input_dir,'historic_shutoff_dates_042022.csv')) %>% filter(subbasin == "bw_ab_magic")

cut.hist<-data.frame("date" = array(NA,c(183)))
cut.hist$date<- dates
cut.hist$doy<- yday(dates) 
  
for (i in 1982:2022){
  cut.hist[,i-1979]<-Reduce(c, lapply(bwh.wy[bwh.wy$wateryear == i, "value"], cutoff, wr.sub))
}

# Selected water rights to retain /test
selected_wr <- as.Date(c("1883-03-24", "1884-10-14", "1886-06-01"))
#cut.hist.bw<- cut.hist %>% mutate(across(3:43, ~ ifelse(. %in% selected_wr, ., NA))) %>% filter(if_any(3:43, ~ !is.na(.)))


#output dataframe will need to look like this
wr_out<-data.frame(wateryear= rep(1982:2022, each = 3), 
               wr= rep(c("1883-03-24", "1884-10-14","1886-06-01"),length.out = length(1982:2022) * 3))

cutoff.yr <- function(wateryear, wr_date, wr_sub){
    wr_date<- as.Date(wr_date)
    flow<- bwh.wy[bwh.wy$wateryear == wateryear, "value"]
    ts <- cutoff(flow, wr_sub)
    #mapply ? or for loop
  }
