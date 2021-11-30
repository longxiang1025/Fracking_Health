##########################################################################
#The objective of this script is to fit Model II of Analysis Set I       #
#In addition to group person-years by PE, each PE group is divided into a#
#upwind subgroup and downwind subgroup. The output of this script is the #
#point estimates of eight HRs of eight wind-dependent exposure groups    #
##########################################################################
library(survival)
library(splines)
library(dplyr)
library(here)
load(here::here("public_data","FIPS_ZIPCODE_Crosswalk.RData"))

print(paste(Sys.time(),"Start Loading Data"))
load(here::here("Temp_Data","Full_Data.RData"))
print(paste(Sys.time(),"Finish Loading"))

#categorize calendar years and age into groups
data=data%>%mutate(year_brks=cut(year,breaks=c(2000,2005,2009,2012,2015),include.lowest=T))
data=data%>%mutate(age_brks=cut(age,breaks = c(65,75,85,95,105),include.lowest = T))

#categorize PE based on quartiles
h_cut_points=quantile(data[data$H_Inv_1st>0,]$H_Inv_1st,c(0.25,0.5,0.75,0.995))
v_cut_points=quantile(data[data$V_Inv_1st>0,]$V_Inv_1st,c(0.25,0.5,0.75,0.995))

#remove the extraordinary high exposure
data=data%>%filter(H_Inv_1st<h_cut_points[4])
data=data%>%filter(V_Inv_1st<h_cut_points[4])

data$h_brks=as.factor((data$H_Inv_1st>0)+(data$H_Inv_1st>h_cut_points[1])+(data$H_Inv_1st>h_cut_points[2])+(data$H_Inv_1st>h_cut_points[3]))
data$v_brks=as.factor((data$V_Inv_1st>0)+(data$V_Inv_1st>h_cut_points[1])+(data$V_Inv_1st>h_cut_points[2])+(data$V_Inv_1st>h_cut_points[3]))

event_terms="Surv(year-1,year,flag_died)~"
strata_terms="strata(race,Dual,Female,year_brks,age_brks)"
linear_terms="pm25+green_ratio+dev_ratio"
categorical_terms="education+poverty+medianhousevalue+popdensity+pct_owner_occ+smoke_rate+mean_bmi+smoke_rate+mean_bmi"

#Calculate the PE contributed by upwind wells
data=data%>%mutate(upwind_ratio=ifelse(H_Inv_1st==0,NA,H_Inv_1st_Up/H_Inv_1st),
                   downwind_ratio=ifelse(H_Inv_1st==0,NA,H_Inv_1st_Down/H_Inv_1st))
#If the relative contribution of upwind wells is > 25%, this zipcode is labelled as downwind to UOGD wells
data=data%>%mutate(up_brks=ifelse(is.na(upwind_ratio),0,(upwind_ratio>=0)+(upwind_ratio>0.25)))
data$up_brks=as.factor(data$up_brks)

#Create a table of coefficient, the unique value of class can be H1U1, H1U2...
combine_table<-expand.grid(h=c(0,1,2,3,4),u=c(0,1,2))
combine_table$Class=paste0("H",combine_table$h,"U",combine_table$u)
combine_table$h<-as.factor(as.character(combine_table$h))
combine_table$u=as.factor(as.character(combine_table$u))
data<-data%>%left_join(combine_table,by=c("h_brks"="h","up_brks"="u"))

data=data%>%dplyr::select(race,year,age,Dual,Female,zipcode,flag_died,pm25,poverty,
                          popdensity,medianhousevalue,medhouseholdincome,pct_owner_occ,smoke_rate,mean_bmi,
                          dev_ratio,green_ratio,education,year_brks,age_brks,Class,v_brks)

formula=paste0(event_terms,strata_terms,"+",linear_terms,"+",categorical_terms,"+Class+v_brks+cluster(zipcode)")
Sys.time()
system.time(m<-coxph(as.formula(formula),data=data))
Sys.time()
write_cox(model=m,output=here::here("doc","Model3.0","Full_Wind_Quantile.txt"),title="Full_Wind_Quantile")

