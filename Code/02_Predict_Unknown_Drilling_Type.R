##################################################################################
#The objective of this script is to predict the drilling type for wells without  #
#valid drilling type information. The input is a file containing information of  #
#all wells (including those without drilling type). The output is a file with the#
#missing drilling types imputed                                                  #
##################################################################################
library(raster)
library(dplyr)
library(lubridate)
library(here)
library(h2o)

prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#Load function files-----------------------------------------------------
source(here::here("code","54_Function_Get_Neghbour_Features.R"))

load(here::here("public_data","Wells","Wells_4th.RData"))
wells$lon=wells$WGS84Longitude
wells$lat=wells$WGS84Latitude
coordinates(wells)<-~WGS84Longitude+WGS84Latitude
proj4string(wells)<-geoprjstring
wells<-spTransform(wells,prjstring)
#Extract the well information from nearby wells with valid information
ngb_types=ngb_features(sp1=wells,sp2 = NULL,feat = "WellboreType",sel=c("H","V"))
wells$Closest_Type=ngb_types$t
wells$H_Num=ngb_types$H_num
wells$V_Num=ngb_types$V_num
wells$DrillType<-as.factor(wells$WellboreType)

wells$Formation[wells$Formation==0]<-NA
wells$Formation[wells$Formation=="NA"]<-NA
wells$Field[wells$Field==0]<-NA
wells$Field[wells$Field=="NA"]<-NA
wells$Field[wells$Field=="(N/A)"]<-NA

wells$WellboreType=as.factor(wells$WellboreType)
rm(ngb_types)

#Initialize the h2o module
h2o.init(nthreads=-1,max_mem_size = "30G",port = (54300))
h2o.data<-as.h2o(wells[wells$WellboreType%in%c("H","V"),])
seed=12345
#Use the auxiliary information as predictors
rfm<-h2o.randomForest(x=c("lon","lat","BasinName","CompletionDate","Field","FluidType",
                          "SpudDate","StateProvince","CurrentStatus","TotalDepth","Closest_Type","H_Num","V_Num",
                          "FirstProdDate","LastProdDate"),
                      y="WellboreType", 
                      nfolds = 10,
                      keep_cross_validation_predictions = F,
                      balance_classes = T,
                      ntrees = 200,
                      max_depth = 20,
                      mtries = 6,
                      stopping_metric = "MAE",
                      min_split_improvement=0.001,
                      stopping_rounds = 2,
                      stopping_tolerance = 0.001,
                      min_rows = 5,
                      training_frame=h2o.data,set.seed(seed))

h2o.data.pred<-as.h2o(wells@data%>%filter(WellboreType%in%c("D","U"))%>%dplyr::select("lon","lat","BasinName","CompletionDate","Field","FluidType",
                                                                          "SpudDate","StateProvince","CurrentStatus","TotalDepth","Closest_Type","H_Num","V_Num",
                                                                          "FirstProdDate","LastProdDate"))
h2o.saveModel(object = rfm,path = here::here("data","RF_Model","RF_Fill_Drill_Type_Model",force=T))
#Use the model to predict drilling type for wells without valid records
drill_type_pred<-h2o.predict(rfm,h2o.data.pred)
drill_type_pred<-as.data.frame(drill_type_pred)
wells<-wells@data
wells$DrillType<-wells$WellboreType
wells[wells$DrillType%in%c("D","U"),"DrillType"]<-drill_type_pred$predict


#######################################################
#we create two trends of the spud-completion duration for vertical wells and horizontal wells
h_wells<-wells%>%filter(!is.na(SpudDate),!is.na(CompletionDate),DrillType=="H",lubridate::year(SpudDate)>2000)
h_wells<-h_wells%>%filter(Dur_Spud_Comp>10,Dur_Spud_Comp<270)
h_trend=h_wells%>%group_by(lubridate::year(SpudDate),lubridate::month(SpudDate))%>%summarise(l=median(Dur_Spud_Comp))
names(h_trend)=c("year","month","length")

v_wells<-wells%>%filter(!is.na(SpudDate),!is.na(CompletionDate),DrillType=="V",lubridate::year(SpudDate)>2000)
v_wells<-v_wells%>%filter(Dur_Spud_Comp>10,Dur_Spud_Comp<270)
v_trend=v_wells%>%group_by(lubridate::year(SpudDate),lubridate::month(SpudDate))%>%summarise(l=median(Dur_Spud_Comp))
names(v_trend)=c("year","month","length")

h_comp_no_spud=wells%>%filter(!is.na(CompletionDate),is.na(SpudDate),CompletionDate>as.Date("2000-01-01"),DrillType=="H")
h_comp_no_spud$year=lubridate::year(h_comp_no_spud$CompletionDate)
h_comp_no_spud$month=lubridate::month(h_comp_no_spud$CompletionDate)
h_comp_no_spud<-h_comp_no_spud%>%left_join(h_trend)
h_comp_no_spud$SpudDate=h_comp_no_spud$CompletionDate-h_comp_no_spud$length
h_comp_no_spud=h_comp_no_spud%>%dplyr::select(names(wells))

v_comp_no_spud=wells%>%filter(!is.na(CompletionDate),is.na(SpudDate),CompletionDate>as.Date("2000-01-01"),DrillType=="V")
v_comp_no_spud$year=lubridate::year(v_comp_no_spud$CompletionDate)
v_comp_no_spud$month=lubridate::month(v_comp_no_spud$CompletionDate)
v_comp_no_spud<-v_comp_no_spud%>%left_join(v_trend)
v_comp_no_spud$SpudDate=v_comp_no_spud$CompletionDate-v_comp_no_spud$length
v_comp_no_spud=v_comp_no_spud%>%dplyr::select(names(wells))

h_spud_no_comp=wells%>%filter(is.na(CompletionDate),!is.na(SpudDate),SpudDate>as.Date("2000-01-01"),DrillType=="H",CurrentStatus=="ACTIVE")
h_spud_no_comp$year=lubridate::year(h_spud_no_comp$SpudDate)
h_spud_no_comp$month=lubridate::month(h_spud_no_comp$SpudDate)
h_spud_no_comp<-h_spud_no_comp%>%left_join(h_trend)
h_spud_no_comp$CompletionDate=h_spud_no_comp$SpudDate+h_spud_no_comp$length
h_spud_no_comp=h_spud_no_comp%>%dplyr::select(names(wells))

v_spud_no_comp=wells%>%filter(is.na(CompletionDate),!is.na(SpudDate),SpudDate>as.Date("2000-01-01"),DrillType=="V",CurrentStatus=="ACTIVE")
v_spud_no_comp$year=lubridate::year(v_spud_no_comp$SpudDate)
v_spud_no_comp$month=lubridate::month(v_spud_no_comp$SpudDate)
v_spud_no_comp<-v_spud_no_comp%>%left_join(v_trend)
v_spud_no_comp$CompletionDate=v_spud_no_comp$SpudDate+v_spud_no_comp$length
v_spud_no_comp=v_spud_no_comp%>%dplyr::select(names(wells))

valid_wells=wells%>%filter(!is.na(SpudDate),!is.na(CompletionDate))
wells<-bind_rows(valid_wells,h_spud_no_comp,h_comp_no_spud,v_spud_no_comp,v_comp_no_spud)
#At the end of day, only 1158238 wells left.
#Solve the issue that some spuddate is behind completion date,too short or too long duration,now there're 320967 wells
ext<-wells%>%filter(SpudDate>CompletionDate|Dur_Spud_Comp<10|Dur_Spud_Comp>270)
normal_wells=wells%>%filter(!(SpudDate>CompletionDate|Dur_Spud_Comp<10|Dur_Spud_Comp>270))
#All spud date are re-calculated in a similar way
h_ext<-ext%>%filter(DrillType=="H")
h_ext$year=lubridate::year(h_ext$CompletionDate)
h_ext$month=lubridate::month(h_ext$CompletionDate)
h_ext=h_ext%>%left_join(h_trend)
h_ext$SpudDate=h_ext$CompletionDate-h_ext$length
h_ext=h_ext%>%dplyr::select(names(wells))

v_ext<-ext%>%filter(DrillType=="V")
v_ext$year=lubridate::year(v_ext$CompletionDate)
v_ext$month=lubridate::month(v_ext$CompletionDate)
v_ext=v_ext%>%left_join(v_trend)
v_ext$SpudDate=v_ext$CompletionDate-v_ext$length
v_ext=v_ext%>%dplyr::select(names(wells))

wells<-bind_rows(normal_wells,h_ext,v_ext)
###############################################


wells<-wells%>%filter(!is.na(SpudDate),!is.na(CompletionDate))
wells<-wells%>%filter(CurrentStatus%in%c("ABANDONED","ACTIVE","COMPLETED","INACTIVE","P & A","SHUT-IN","TA"))
#Add a column called EndDate, indicating the end of study 2019-09-01
wells$EndDate=NA
wells$EndDate=as.Date(wells$EndDate)
#Current Active Wells, in activate time is 2019-09-01
act_wells<-wells%>%filter(CurrentStatus=="ACTIVE")%>%mutate(EndDate=replace(EndDate,CurrentStatus=="ACTIVE","2019-01-01"))
#Current completed wells, if production exists after 2019-01-01, enddate is its last production date,
#if production ends before 2000-01-01, remove, if production ends between, set it as end date
comp_wells<-wells%>%filter(CurrentStatus%in%c("COMPLETED"))
#if before 2000 and no production, remove
comp_wells=comp_wells[!(comp_wells$SpudDate<"2000-01-01"&is.na(comp_wells$OnProductionDate)),]
# if some production, keep if no matter whether it's before 2000-01-01
comp_wells[!is.na(comp_wells$OnProductionDate),"EndDate"]=comp_wells[!is.na(comp_wells$OnProductionDate),"LastProdDate"]
# if after 2000, and no production, keep as unfinished.
comp_wells[is.na(comp_wells$OnProductionDate),"EndDate"]=NA
#Current Inactive wells, find the last productuon date
inactive_wells<-wells%>%filter(CurrentStatus%in%c("INACTIVE","ABANDONED","P & A","SHUT-IN","TA"))
#Current Inactive wells without production dates are removed
inactive_wells=inactive_wells%>%filter(!is.na(OnProductionDate))
inactive_wells$EndDate=inactive_wells$CurrentStatusDate


wells<-bind_rows(act_wells,comp_wells,inactive_wells)
wells<-wells%>%filter(EndDate>"2000-01-01"|is.na(EndDate))



save(file=here::here("public_data","Wells","Wells_4th.RData"),wells)
