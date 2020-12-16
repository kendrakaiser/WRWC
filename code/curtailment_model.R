# ---------------------------------------------------------------------------- #
# WRWC Curtailment Date Model
# Kendra Kaiser
# December, 16 2020

cd <<- '~/Desktop/Data/WRWC'
volumes<-read.csv(file.path(cd,"vol.sample.csv")) #ac-ft
curtailments<- read.csv(file.path(cd,"historic_shutoff_dates_071520.csv"))
var<-read.csv(file.path(cd,'April1_vars.csv'))

key <- unique(curtailments[c("subbasin", 'water_right_cat')])

for (i in 1:dim(key)[1]){
curt<- curtailments %>% filter(subbasin == key[i,1] & water_right_cat == key[i,2]) %>% select(year, shut_off_julian)
plot(var$bwb.vol.nat[var$year <2020], curt$shut_off_julian[bw.a.a$year >= 1988])
}
