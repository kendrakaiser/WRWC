# Data Integration
# merge data from data scraping and temp models
# Kendra Kaiser
# 09-15-2023

# Import & Compile Data -------------------------------------------------------# 
# Streamflow, Current SWE, historic Temperature Data

# input data name 'alldat_mo.csv'
if (month(end_date) == 2){
  swe_q  <<- alldat_feb
} else if (month(end_date) == 3){
  swe_q  <<- alldat_mar
} else if (month(end_date) == 4){
  swe_q  <<- alldat_april
}

swe_q[swe_q == 0] <- 0.01 # change zeros to a value so lm works 

# combine discharge & SWE with temp data
var = swe_q %>% inner_join(all.temp.dat, by ="wateryear")

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
var<-var[,!(names(var) %in% c('cg.swe', 'g.swe','gs.swe','hc.swe', 'lwd.swe','ga.swe','bc.swe', 'sr.nj_t', 'sr.aj_t'))]

