# import and analyze water right data in WD37
# July 13th, 2022
# Kendra Kaiser
# ---------------------------------

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

#-------------------------------------------------------------------------------
#EVALUAION
#-------------------------------------------------------------------------------

#run historic hydrographs through the curtailment model to assess how well it works 
#changed the original file, it had everything as 10-14-1884, but should have been 10-15
wr_obs<-read.csv(file.path(input_dir,'historic_shutoff_dates_042022.csv')) %>% filter(subbasin == "bw_ab_magic")
wr_obs<- wr_obs %>% rename(wateryear = year) %>% mutate(wr= as.Date(wr_obs$water_right_date, format = "%m/%d/%Y"))

cut.hist<-data.frame("date" = array(NA,c(183)))
cut.hist$date<- dates
cut.hist$doy<- yday(dates) 
  
for (i in 1982:2022){
  cut.hist[,i-1979]<-Reduce(c, lapply(bwh.wy[bwh.wy$wateryear == i, "value"], cutoff, wr.sub))
}

# Selected water rights to retain /test
selected_wr <- as.Date(c("1883-03-24", "1884-10-15", "1886-06-01"))
#cut.hist.bw<- cut.hist %>% mutate(across(3:43, ~ ifelse(. %in% selected_wr, ., NA))) %>% filter(if_any(3:43, ~ !is.na(.)))


#limit the scope of search to after May 
bwh.short <- bwh.wy %>% filter(yday(datetime)>140)

cutoff.yr <- function(wateryear, wr_date, wr.sub){
    wr_date<- as.Date(wr_date)
    doy<-yday(seq(as.Date(paste(wateryear,"-05-20",sep="")),as.Date(paste(wateryear,"-09-30",sep="")),"day"))
    flow<- data.frame(cfs=bwh.short[bwh.short$wateryear == wateryear, "value"]) # May 20
    ts <- as.Date(Reduce(c, lapply(flow$cfs, cutoff, wr.sub)))
    
    if (length(which(ts == wr_date) > 0)){
      cutoff.dates <- min(doy[which(ts == wr_date)])
    } else {cutoff.dates <- NaN}
}


#create dataframe to hold results
wr_out<-data.frame(wateryear= rep(1982:2022, each = 3), 
                   wr= rep(c("1883-03-24", "1884-10-15","1886-06-01"),length.out = length(1982:2022) * 3))

#determine cutoff dates for historic hydrographs; 
#TODO: for some reason it isn't including the 10/18/1884 water right, but wr_out$wr is fed into the function
wr_out$cutoff_date <- mapply(cutoff.yr, wateryear = wr_out$wateryear,
                             wr_date = wr_out$wr, MoreArgs = list(wr.sub = wr.sub))
wr_out <- wr_out %>% na.omit() %>% mutate(wr = as.Date(wr))

# Merge datasets based on 'wr'
wr_out <- wr_out %>% 
  left_join(wr_obs %>% dplyr::select(wateryear, wr, shut_off_julian, 
      water_right_date), by = c("wateryear", "wr"))%>% na.omit()
# Define the range for both axes
julian_range <- range(wr_out$shut_off_julian, wr_out$cutoff_date, na.rm = TRUE)
# Create weekly breaks
weekly_breaks <- seq(julian_range[1], julian_range[2], by = 14)
# Define a function to format Julian dates as MM-DD
julian_to_date_label <- function(julian) {
  format(as.Date(julian, origin = "2000-12-31"), "%m-%d")  # Using a non-leap year as reference
}

c<- ggplot(wr_out, aes(x=shut_off_julian, y=cutoff_date, color=water_right_date)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  geom_point()+
  theme_bw()+
  scale_x_continuous(labels = julian_to_date_label, breaks = weekly_breaks,  limits = julian_range) +
  scale_y_continuous(labels = julian_to_date_label, breaks = weekly_breaks, limits = julian_range) +
  labs(x = "Actual Curtailment Date", y = "Predicted Curtailment Date", color = "Water Right")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))

png(filename = file.path("curtailment_evaluation.png"),
    width = 6.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
print(c)
dev.off()
