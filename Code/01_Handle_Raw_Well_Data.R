##################################################################################
#The aim of this script is to clean and raw data downloaded directly from Enverus#
#The input is the county-specific file from Enverus, the output is the cleaned d-#
#ata                                                                             #
#Please note that Enverus may change the data format without notice in advance   #
##################################################################################
library(here)
library(dplyr)
library(lubridate)
library(ggplot2)
well_prod_files<-list.files(here::here("public_data","Wells","Well_Production_Data_0829"),full.names = T)
wells_list<-list()
#iterate through the raw files, which are downloaded by county (FIPS)
for(f in 1:length(well_prod_files)){
  load(well_prod_files[f])
  #For each county, we only select wells with spatiotemporal infromation
  wells<-wells%>%filter(!(is.na(SpudDate)&is.na(CompletionDate)&is.na(OnProductionDate)))
  wells<-wells%>%filter(!is.na(WGS84Latitude))
  wells_list[[f]]=wells
  if(f%%50==0){
    print(paste(Sys.time(),f,"Out of",length(well_prod_files)," has been loaded"))
  }
}
wells<-bind_rows(wells_list)
#1)Only keep the induplicated record
wells<-wells%>%distinct()
#2976787 wells are left

# select based on product type --------------------------------------------
#Only keep wells with actual production, exluding disposal, dry well and other types of wells
wells<-wells%>%filter(FluidType%in%c("CYCLIC STEAM",
                                     "GAS",
                                     "GAS OR COALBED",
                                     "OIL",
                                     "OIL & GAS"))
#2335235 wells left. Most of the wells are Oil (1382908)
summary(as.factor(wells$FluidType))

# select based on current status ------------------------------------------
#remove any wells ABANDONED, INACTIVE, ORPHAN, P & A, SHUT-IN, TA before 01/01/2000
wells<-wells%>%filter(CurrentStatus!="UNKNOWN")
wells$CurrentStatusDate=as.Date(wells$CurrentStatusDate)
wells=wells%>%mutate(CurrentStatusDate=replace(CurrentStatusDate,
                                               CurrentStatusDate>"2100-01-01",
                                               "2007-08-17"))
#use the last production date to fill some date of abandoned, 234470 wells abandoned without abandent time, but 100793 have last production date
t<-wells%>%filter(!CurrentStatus%in%c("ACTIVE","COMPLETED","DRILLED"),is.na(CurrentStatusDate),!is.na(LastProdDate))
wells[is.na(wells$CurrentStatusDate)&!wells$CurrentStatus%in%c("ACTIVE","COMPLETED","DRILLED"),"CurrentStatusDate"]=
  wells[is.na(wells$CurrentStatusDate)&!wells$CurrentStatus%in%c("ACTIVE","COMPLETED","DRILLED"),"LastProdDate"]
#273784 wells are with unknown status date, out of them 33547 are active. Only them are kept
t<-wells%>%filter(is.na(CurrentStatusDate))
wells<-wells%>%filter((!is.na(CurrentStatusDate))|(CurrentStatus%in%c("ACTIVE","COMPLETED","DRILLED")))
#2201549 wells left. The wells without status update date were set as 2019-09-01
wells<-wells%>%mutate(CurrentStatusDate=replace(CurrentStatusDate,
                                       (CurrentStatus%in%c("ACTIVE","COMPLETED","DRILLED")),
                                       as.Date("2019-09-01")))
#wells with absurd current update date are adjusted
wells[wells$CurrentStatusDate>"2019-09-01","CurrentStatusDate"]=wells[wells$CurrentStatusDate>"2019-09-01","CurrentStatusDate"]-14600

#wells abandoned before 2001-01-01 are excluded from the dataset
wells=wells%>%filter(!(CurrentStatusDate<"2001-01-01"&!(CurrentStatus%in%c("ACTIVE","COMPLETED","DRILLED"))))
#1385847 wells left. Waht's left are currently active wells or wells abandoned after 2001-01-01
# select based on spuddata completion date and production date ------------
#Use the entitiy production date to fill the missing production date of well bore
wells[is.na(wells$OnProductionDate),"OnProductionDate"]=wells[is.na(wells$OnProductionDate),"FirstProdDate"]
# use the production date to fill the missing part of completion date
wells[is.na(wells$CompletionDate),"CompletionDate"]=wells[is.na(wells$CompletionDate),"OnProductionDate"]
# there's remarkable regional difference in the data quality, especially the spud date
t<-wells%>%group_by(StateProvince)%>%summarize(mean(is.na(SpudDate)),mean(is.na(CompletionDate)))
print(as_tibble(t),n=30)
#The only horizontal well with spuddate after completion date is visually checked and adjusted
wells=wells%>%mutate(SpudDate=replace(SpudDate,
                                      (SpudDate>CompletionDate&WellboreType=="H"),
                                      as.Date("2015-06-02")))
# calculate the duration --------------------------------------------------
#14136 wells with valid spud date but no completion date
wells$Dur_Spud_Comp=difftime(wells$CompletionDate,wells$SpudDate,units = "days")
wells$Dur_Spud_Comp=as.numeric(wells$Dur_Spud_Comp)

#199532 wells with completion date but no spudding date, we need to estimate the construction duration for them
t<-wells%>%filter(!is.na(CompletionDate),is.na(SpudDate),CompletionDate>as.Date("2000-01-01"))
#14136 wells with spud date but no completion date, only select the active ones
t<-wells%>%filter(is.na(CompletionDate),!is.na(SpudDate),SpudDate>as.Date("2000-01-01"))


save(file=here::here("public_data","Wells","Wells_4th.RData"),wells)
