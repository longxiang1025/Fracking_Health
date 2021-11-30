######################################################################
#The objective of this script is to merge exposure with Medicare data#
#This script has been modified to run on the research computing envi-#
#ronment (RCE), which is an IQSS-based research computing resource   #
#for confidential health data                                        #
#The output of this script is county-specific files including all ne-#
#cessary exposure and outcome information                            #
######################################################################
f=as.integer(as.character(Sys.getenv("NID")))
print(f)
library(readr)
library(stringr)
library(dplyr)
medicare_files=list.files(here::here("FIPS_MDC"),full.names=T)
load(medicare_files[f])
fips=unique(dat$FIPS)
county_fips=fips
#If the county-specific file exists for some reason, this step can be skipped
if(!file.exists(here::here("GeoData","Medicare_Exp_FIPS2.0",paste0(fips,".RData")))){
  ##Load the county-specific environmental exposure data including pm25 and highest monthly average temperature
  load(here::here("GeoData","Env_Exp.RData"))
  ##Load the behavior risk dataset
  dat.brfss <- read_csv(here::here("GeoData","brfss_interpolated.csv"), 
                        col_types = cols(
                          fips = col_character(),
                          smoke_rate = col_double(),
                          mean_bmi = col_double(),
                          year = col_double(),
                          ZIP = col_character())) %>%
    mutate(ZIP = stringr::str_pad(ZIP, width = 5, side = "left", pad = 0)) %>%
    dplyr::select(-fips)
  ##Load the socioeconomic variables
  env_exp=env_exp%>%dplyr::select(year,ZIP,pm25,poverty,popdensity,medianhousevalue,medhouseholdincome,
                                  population,tmmx,pct_blk,pct_owner_occ,hispanic,education)
  
  load(here::here("public_data","ses_appen.RData"))
  names(ses_appen)[1]<-"zipcode"
  
  load(here::here("public_data","Green_Exp.RData"))
  load(here::here("public_data","Frack_Exp_2nd.RData"))
  ##Separate all medicare record into two parts, full record and partial record
  t<-dat%>%group_by(QID)%>%summarise(entry_year=min(year),last_year=max(year),yod=mean(yod),l=last_year-entry_year+1,nrecord=length(QID))
  ##full records don't need any further process
  full_record=t%>%filter(l==nrecord)
  full_record=dat%>%filter(QID%in%full_record$QID)
  print(paste0(nrow(full_record)/nrow(dat)," of the medicare in this county is complete"))
  ##partial record means number of years larger than record count,meaning the health status of some years are missing
  partial_record=t%>%filter(l>nrecord)
  partial_list=list()
  print(paste0(nrow(partial_record)," need to be filled"))
  if(nrow(partial_record)>0){
    for(p in 1:nrow(partial_record)){
      ###The aim of this loop is to fill the missing years
      p_record=dat%>%filter(QID==as.character(partial_record[p,"QID"]))%>%arrange(year)
      ### the entry year is detected first
      entry_age=min(p_record$age)
      entry_year=min(p_record$year)
      ### the ending year is detected
      left_year=max(p_record$year)
      left_age=entry_age+left_year-entry_year
      f_record=cbind.data.frame(entry_age:left_age,entry_year:left_year)
      names(f_record)=c("age","year")
      ### if there're any years between entry and ending years but without health record, impute them
      mis_record=f_record%>%left_join(p_record,by=c("age","year"))
      mis_record[is.na(mis_record$QID),!names(mis_record)%in%c("age","year")]=
        mis_record[1,!names(mis_record)%in%c("age","year")]
      partial_list[[p]]=mis_record
    }
    partial_record=bind_rows(partial_list)
    print("Partial records are filled")
    fips_all_data=bind_rows(full_record,partial_record)
  }else{
    fips_all_data=full_record
  }

  frack_expo=frack_expo%>%dplyr::filter(fips==county_fips)
  fips_all_data=dat%>%dplyr::select(QID,race,year,age,Dual,Female,zipcode,yod,flag_died,statecode)%>%
    left_join(env_exp,by=c("year"="year","zipcode"="ZIP"))%>%
    left_join(frack_expo,by=c("year"="year","zipcode"="zipcode"))%>%
    left_join(dat.brfss,by=c("year"="year","zipcode"="ZIP"))%>%
    left_join(ses_appen,by=c("year"="year","zipcode"="zipcode"))%>%
    left_join(zipcode_green,by=c("year"="year","zipcode"="zipcode"))%>%
    dplyr::filter(!is.na(vars))
  fips_all_data$race=as.factor(fips_all_data$race)
  ##detect the entry year of each participant (a bit redundant, but not a mistake)
  entry_year=fips_all_data%>%group_by(QID)%>%summarise(entry_year=min(year))
  fips_all_data<-fips_all_data%>%left_join(entry_year)
  
  entry_age=fips_all_data%>%group_by(QID)%>%summarise(entry_age=min(age))
  fips_all_data<-fips_all_data%>%left_join(entry_age)
  
  ##convert data from float/double to integer to save space
  fips_all_data$tmmx=as.integer(fips_all_data$tmmx)
  fips_all_data$pm25=as.integer(fips_all_data$pm25)
  fips_all_data$year=as.integer(fips_all_data$year)
  fips_all_data$age=as.integer(fips_all_data$age)
  
  fips_all_data=fips_all_data%>%filter(entry_age>64)
  fips_all_data=fips_all_data%>%select(-yod)
  
  save(file=here::here("GeoData","Medicare_Exp_FIPS2.0",paste0(fips,".RData")),fips_all_data)
}
