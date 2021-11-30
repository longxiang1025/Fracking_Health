#########################################################################
#The main script to run the cox proportional hazard model regardless of #
#wind direction (Model I of Analysis Set I)                             #
#########################################################################
library(survival)
library(dplyr)
library(here)
sessionInfo()

#Load the necessary data.
#If the temporary file is not pre-made, the previous code need to be run first
print(paste(Sys.time(),"Start Loading Data"))
load(here::here("Temp_Data","Full_Data.RData"))
print(paste(Sys.time(),"Finish Loading"))

#Cut the PE into four categories with quartiles, the extremely high exposure
h_cut_points=quantile(data[data$H_Inv_1st>0,]$H_Inv_1st,c(0.25,0.5,0.75,0.995))
v_cut_points=quantile(data[data$V_Inv_1st>0,]$V_Inv_1st,c(0.25,0.5,0.75,0.995))

data=data%>%filter(H_Inv_1st<h_cut_points[4])
data=data%>%filter(V_Inv_1st<h_cut_points[4])

data$h_brks=as.factor((data$H_Inv_1st>0)+(data$H_Inv_1st>h_cut_points[1])+(data$H_Inv_1st>h_cut_points[2])+(data$H_Inv_1st>h_cut_points[3]))
data$v_brks=as.factor((data$V_Inv_1st>0)+(data$V_Inv_1st>h_cut_points[1])+(data$V_Inv_1st>h_cut_points[2])+(data$V_Inv_1st>h_cut_points[3]))

data=data%>%mutate(year_brks=cut(year,breaks=c(2001,2005,2009,2012,2015),include.lowest=T))
data=data%>%mutate(age_brks=cut(age,breaks = c(65,75,85,95,105),include.lowest = T))

data=data%>%dplyr::select(race,year,age,Dual,Female,zipcode,flag_died,pm25,poverty,education,
                          popdensity,medianhousevalue,medhouseholdincome,pct_owner_occ,smoke_rate,mean_bmi,
                          dev_ratio,green_ratio,h_brks,v_brks,year_brks,age_brks)

event_terms="Surv(year-1,year,flag_died)~"
strata_terms="strata(race,Dual,Female,year_brks,age_brks)"
linear_terms="pm25+green_ratio+dev_ratio+"
categorical_terms="education+poverty+medianhousevalue+medhouseholdincome+popdensity+pct_owner_occ+smoke_rate+mean_bmi+"
formula=paste0(event_terms,strata_terms,"+",linear_terms,"+",categorical_terms,"+h_brks+v_brks+cluster(zipcode)")

Sys.time()
system.time(m<-coxph(as.formula(formula),data=data))
Sys.time()
write_cox(model=m,output=here::here("doc","Model3.0","Full_Cox_Quantile.txt"),title="Full_Model")
