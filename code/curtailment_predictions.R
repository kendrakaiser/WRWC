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



