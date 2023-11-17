# Data Integration
# merge data from data scraping and temp models
# Kendra Kaiser
# 09-15-2023

# Import & Compile Data -------------------------------------------------------# 

# Streamflow, Current SWE, historic and Modeled Temperature Data
usgs_sites = read.csv(file.path(data_dir,'usgs_sites.csv'))
stream.id<-unique(as.character(usgs_sites$abv))

# input data name 'alldat_mo.csv'
swe_q = read.csv(file.path(data_dir,input_data))
swe_q$bws.vol[swe_q$year <=1996]<- NA
swe_q[swe_q == 0] <- 0.01 # change zeros to a value so lm works 

# observed temperatures
spring.temps = read.csv(file.path(data_dir, 'sprTemps.csv'))
nj.temps = read.csv(file.path(data_dir, 'njTemps.csv'))

# combine discharge & SWE with temp data
var = swe_q %>% inner_join(spring.temps, by ="year") %>% inner_join(nj.temps, by ="year")

# sp.test<-apply(var, MARGIN=2, shapiro.test) 
#TODO : automate this step (e.g. if p < 0.05 log it and remove the og)
# normailze parameters that have a shapiro.test() < 0.05
var$cg.log_swe <- log(var$cg.swe)
var$g.log_swe <- log(var$g.swe)
var$gs.log_swe <- log(var$gs.swe)
var$hc.log_swe <- log(var$hc.swe)
var$lwd.log_swe <- log(var$lwd.swe)
var$ga.log_swe <- log(var$ga.swe)
var$bc.log_swe <- log(var$bc.swe)
var<-var[,!(names(var) %in% c('cg.swe', 'g.swe','gs.swe','hc.swe', 'lwd.swe','ga.swe','bc.swe', 'nj_t.sr', 'aj_t.sr'))]

#save variables for use in other scripts
#TODO: change folder structure so that they are either pre-defined or created
write.csv(var, file.path(model_out,'all_vars.csv'), row.names = FALSE)
