################################################################
#The objective of this script is to perform Analysis Set II    #
#The inputs of this script is identical to Analysis Set I      #
#We defined the intervention, treatment/comparison groups, and #
#used two regression models to perform the DiD and DDD analysis#
################################################################
setwd("~/shared_space/ci3_lol087/Fracking_Health")
library(dplyr)
library(here)
library(lfe)

print(paste(Sys.time(),"Start Loading Data"))
load(here::here("Temp_Data","Full_Data_NBrks.RData"))
load(here::here("code","Formulas.RData"))
print(paste(Sys.time(),"Finish Loading"))

cut_points=quantile(data[data$H_Inv_1st>0,]$H_Inv_1st,c(0.25,0.5,0.75,0.99))
data$brks=as.factor((data$H_Inv_1st>0)+(data$H_Inv_1st>cut_points[1])+(data$H_Inv_1st>cut_points[2])+(data$H_Inv_1st>cut_points[3]))

#Due to the definition of treatment/comparison group, we need to know the PE at the end of
#study period, we assume a monotonic increase in UOGD exposure, so we use the max level
zip_exp=data%>%group_by(zipcode)%>%summarise(max_exp=max(H_Inv_1st),
                                             min_exp=min(H_Inv_1st),
                                             Up=mean(H_Inv_1st_Up/H_Inv_1st,na.rm=T),
                                             Down=mean(H_Inv_1st_Down/H_Inv_1st,na.rm=T))
#The treatment group is defined as the medium-high and high group at the end of study period
#The upwind subgroup is defined as zipcodes with upwind contribution >0.5
high_up_zips=zip_exp%>%filter(max_exp>cut_points[2],max_exp<cut_points[4],Up>=0.5)
high_down_zips=zip_exp%>%filter(max_exp>cut_points[2],max_exp<cut_points[4],Down>=0.5)

#The definition of comparison group and subgroups
low_up_zips=zip_exp%>%filter(max_exp>0,max_exp<cut_points[2],Up>=0.5)
low_down_zips=zip_exp%>%filter(max_exp>0,max_exp<cut_points[2],Down>=0.5)

#We recreate the dataset for analysis by merging records from treatment and comparison groups
high_up_data=data%>%filter(zipcode%in%high_up_zips$zipcode)
high_up_data$trt=T
high_up_data$Up=T

high_down_data=data%>%filter(zipcode%in%high_down_zips$zipcode)
high_down_data$trt=T
high_down_data$Up=F

low_up_data=data%>%filter(zipcode%in%low_up_zips$zipcode)
low_up_data$trt=F
low_up_data$Up=T

low_down_data=data%>%filter(zipcode%in%low_down_zips$zipcode)
low_down_data$trt=F
low_down_data$Up=F

t1_data=bind_rows(high_up_data,high_down_data,low_up_data,low_down_data)
#Drilling year is defined as the year when PE gets above 0
t1_post_year=t1_data%>%filter(H_Inv_1st>0)%>%group_by(zipcode)%>%summarise(post_year=min(year))
t1_data=t1_data%>%left_join(t1_post_year)
#dif_year>0 means post-drilling and dif_year<0 means pre-drilling
t1_data$dif_year=t1_data$year-t1_data$post_year
t1_data$post=t1_data$dif_year>=0

t1_data$medianhousevalue=t1_data$medianhousevalue/1000
t1_data$popdensity=t1_data$popdensity/1000
t1_data$tmmx=t1_data$tmmx-273
t1_data$V_Inv_1st=(t1_data$V_Inv_1st-mean(t1_data$V_Inv_1st))/sd(t1_data$V_Inv_1st)
t1_data$year=t1_data$year-2000
t1_data$age=t1_data$age-60
t1_data$zipcode=as.factor(t1_data$zipcode)
t1_data=t1_data[,c("QID","race","year","age","Dual","Female","flag_died","pm25","poverty","popdensity",
                   "tmmx","fips","smoke_rate","mean_bmi","trt","Up","post_year","post","zipcode","dif_year")]
t1_data=t1_data%>%unique()
rm(data,high_down_data,high_up_data,low_down_data,low_up_data,zips)

t1_data$drill=t1_data$dif_year==0

#The DiD analysis. We assign an individual-level fixed effects(QID).It is also used to calculate cluster robust standard errors
m1=felm(formula=flag_died~trt*post+Dual+Female+age+year+
          pm25+tmmx+poverty+popdensity+smoke_rate+mean_bmi|QID|0|QID,data=t1_data)
#The DDD analysis, the same structure of factor/cluster is used.
m1.1=felm(formula=flag_died~trt*post*Up+Dual+Female+age+year+
            pm25+tmmx+poverty+popdensity+smoke_rate+mean_bmi|QID|0|QID,data=t1_data)
