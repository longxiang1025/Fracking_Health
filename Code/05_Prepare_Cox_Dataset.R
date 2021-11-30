#########################################################################
#The objective of this script is to prepare the dataset for Cox model####
#The input of this script includes well_type ("a_h","a_v","c_h","c_v")  #
#and the exposure and Medicare data                                     #
#The output of this script includes four ready-to-analyze files with re-#
#spect to each type of well                                             #
#########################################################################
library(readr)
library(stringr)
library(dplyr)

print(Sys.time())
print("Finish loading all packages")
load(here::here("public_data","State_County_Table.RData"))
load(here::here("public_data","FIPS_ZIPCODE_Crosswalk.RData"))
load(here::here("public_data","Basic_Geodata","Play_County_Table_75.RData"))
print("Finish loading basic data")
print(Sys.time())

#There're four types of wells "a_h" means active horizontal wells, "c_h" means horizontal wells under construction.
#"a_v" means active vertical wells, "c_v" means vertical wells under construction.
type="a_h"
data<-list()
for(i in 1:nrow(play_county_table)){
  fips=play_county_table[i,"FIPS"]
  if(file.exists(here::here("GeoData","Medicare_Exp_FIPS3.0",paste0(fips,".RData")))){
    load(here::here("GeoData","Medicare_Exp_FIPS3.0",paste0(fips,".RData")))
    fips_all_data=fips_all_data%>%filter(vars==type)
    fips_all_data<-fips_all_data%>%select(-c(statecode,HspPct,PctA1c,population,BlkPct,vars,pct_blk,hispanic))%>%mutate(poverty=as.integer(100*poverty),
                                          popdensity=as.integer(popdensity),
                                          medianhousevalue=as.integer(medianhousevalue),
                                          medhouseholdincome=as.integer(medhouseholdincome),
                                          pct_owner_occ=as.integer(100*pct_owner_occ),
                                          smoke_rate=as.integer(100*smoke_rate),
                                          mean_bmi=as.integer(mean_bmi),
                                          PctEye=as.integer(PctEye),
                                          PctLDL=as.integer(PctLDL),
                                          Pctmam=as.integer(Pctmam),
                                          LungCancerRate=as.integer(10000*LungCancerRate),
                                          dev_ratio=as.integer(100*dev_ratio),
                                          green_ratio=as.integer(100*green_ratio),
                                          entry_year=as.integer(entry_year),
                                          entry_age=as.integer(entry_age),
                                          education=as.integer(100*education))
    n=nrow(fips_all_data)/4
    fips_all_data=fips_all_data[1+4*(0:(n-1)),]
    fips_all_data=fips_all_data%>%dplyr::filter(rowSums(is.na(fips_all_data))==0)
    fips_all_data=fips_all_data%>%dplyr::filter(age>64&age<106)
    data[[i]]=fips_all_data
    print(paste(Sys.time(),i,"out of",nrow(play_county_table),":",play_county_table[i,"FIPS"],play_county_table[i,"NAME"],play_county_table[i,"play"],nrow(fips_all_data)))
  }
}
data<-bind_rows(data)
data=data%>%dplyr::filter(rowSums(is.na(data))==0)
# A file without categorizing the covariates is saved to produce Table 1.
save(file=paste0(here::here("Temp_Data"),"/Full_Data_NBrks",type,".RData"),data)

#We categorized covariates to capture in non-linear relationship with mortality.
data<-data%>%mutate(poverty= cut(poverty, breaks = quantile(poverty, probs = seq(0, 1, 0.25)),include.lowest = TRUE))
#population density is also time variant, So categorize and stratify based on median
data<-data%>%mutate(popdensity = cut(popdensity, breaks = quantile(popdensity, probs = seq(0, 1, 0.25)),include.lowest = TRUE))
#median house value is non-linearly related with mortality, so categorize but not stratify
data<-data%>%mutate(medianhousevalue= cut(medianhousevalue, breaks = quantile(medianhousevalue, probs = seq(0, 1, 0.25)),include.lowest = TRUE))
data<-data%>%mutate(medhouseholdincome = cut(medhouseholdincome, breaks = quantile(medhouseholdincome, probs = seq(0, 1, 0.25)),include.lowest = TRUE))
# percent occupied by owner is also non-linear related, so categorize based on median
data<-data%>%mutate(pct_owner_occ = cut(pct_owner_occ, breaks = quantile(pct_owner_occ, probs = seq(0, 1, 0.25)),include.lowest = TRUE))
data<-data%>%mutate(education = cut(education, breaks = quantile(education, probs = seq(0, 1, 0.25)),include.lowest = TRUE))
data=data%>%mutate(smoke_rate=cut(smoke_rate, breaks = quantile(smoke_rate, probs = seq(0, 1, 0.25)),include.lowest = TRUE))
data=data%>%mutate(mean_bmi=cut(mean_bmi, breaks = quantile(mean_bmi, probs = seq(0, 1, 0.25)),include.lowest = TRUE))

data=data%>%mutate(PctLDL=cut(PctLDL, breaks = quantile(PctLDL, probs = seq(0, 1, 0.25)),include.lowest = TRUE))
data=data%>%mutate(PctEye=cut(PctEye, breaks = quantile(PctEye, probs = seq(0, 1, 0.25)),include.lowest = TRUE))
data=data%>%mutate(Pctmam=cut(Pctmam, breaks = quantile(Pctmam, probs = seq(0, 1, 0.25)),include.lowest = TRUE))
data=data%>%mutate(LungCancerRate=cut(LungCancerRate, breaks = quantile(LungCancerRate, probs = seq(0, 1, 0.25)),include.lowest = TRUE))

data=data%>%mutate(dev_ratio=cut(dev_ratio, breaks = quantile(dev_ratio, probs = seq(0, 1, 0.25)),include.lowest = TRUE))
data=data%>%mutate(green_ratio=cut(green_ratio, breaks = quantile(green_ratio, probs = seq(0, 1, 0.25)),include.lowest = TRUE))
data=data%>%dplyr::filter(rowSums(is.na(data))==0)

save(file=paste0(here::here("Temp_Data"),"/Full_Data_",type,".RData"),data)

