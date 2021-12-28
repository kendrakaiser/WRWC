```{r HistoricStreamflowData, echo=FALSE, fig.show='hold', out.width="50%"}
#png(filename = file.path(fig_dir,"historic_streamflow.png"),
#   width = 5.5, height = 5.5,units = "in", pointsize = 12,
#  bg = "white", res = 600, type ="quartz") 
#dev.off()
par(mar = c(4, 4, .1, .1))
dat<-q %>% filter(abv != 'sc')
ggplot(data = dat, mapping = aes(x=as.Date(Date), y=Flow))+
  labs(title = "Historic Data: Big Wood River and Camas Creek") +
  geom_line(aes(color= abv))+
  labs(color = "Site")+
  scale_colour_discrete(labels = c("Big Wood Hailey", "Big Wood Stanton Crossing", 
                                   "Camas Creek")) +
  xlab('Year')+
  ylab('Flow (cfs)') +
  theme_bw() +
  theme(legend.position = c(.18, .84)) 

dat<-q %>% filter(abv == 'sc') 
ggplot(data = dat, mapping = aes(x=as.Date(Date), y=Flow))+
  labs(title = "Historic Data: Silver Creek at Sportsman's Access ") +
  geom_line(show.legend = FALSE)+
  xlab('Year')+
  ylab('Flow (cfs)')+
  theme_bw()

```





