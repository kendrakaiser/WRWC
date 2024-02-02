# Data Integration
# merge data from data scraping and temp models
# Kendra Kaiser
# 09-15-2023

# Import & Compile Data -------------------------------------------------------# 
# Streamflow, Current SWE, historic Temperature Data

# input data name 'alldat_mo.csv'
if (run_date == 'feb1'){
  swe_q  <<- alldat_feb
} else if (run_date == 'march1'){
  swe_q  <<- alldat_mar
} else if (run_date == 'april1'){
  swe_q  <<- alldat_april
}

swe_q[swe_q == 0] <- 0.01 # change zeros to a value so lm works 

# observed temperatures
#temps = read.csv(file.path(data_dir, "temp_dat.csv"))

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


#save variables for use in other scripts
#TODO: change folder structure so that they are either pre-defined or created -- alt to check w sam about -- remove saving all together because they will stay in the environment?
#write.csv(var, file.path(model_out,'all_vars.csv'), row.names = FALSE)
