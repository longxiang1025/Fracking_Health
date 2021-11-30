############################################################################
#The objective of this script is to:
#(1) find the spatial feature of the zipcode/post
#(2) extract the population density within a buffer of that spatial feature
#(3) calculate the exposure assessment of interest of that zipcode/post
############################################################################
#Get the job id from batch shell
sim=as.integer(as.character(Sys.getenv("Sim")))
setwd("/n/koutrakis_lab/lab/Fracking_Health/")
for(s in (5*sim):(5*sim+4)){
  # Try to arrange the jobs as random as possible
  year=2001+as.integer(s/3108)
  num=1+s%%3108
  print(year)
  print(num)
  library(here)
  library(raster)
  library(stringr)
  library(rgeos)
  library(dplyr)
  library(sp)
  library(lubridate)
  library(RANN)
  library(sf)
  
  print(Sys.time())
  print("Finish loading all packages")
  source(here::here("code","00_Function_Calculate_Area_Based_Density.R"))
  source(here::here("code","00_Function_Prepare_Wind.R"))
  temp_path="/n/scratchlfs/koutrakis_lab/Temp_Data/"
  prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
  geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  load(here::here("public_data","State_County_Table.RData"))
  load(here::here("public_data","FIPS_ZIPCODE_Crosswalk.RData"))
  print(Sys.time())
  print(s_c_table[num,])
  FIPS_ZIPCODE_TABLE<-FIPS_ZIPCODE_TABLE%>%filter(zips!="(Other)")
  print(FIPS_ZIPCODE_TABLE%>%filter(fips==s_c_table[num,"FIPS"]))
  ##an annual for loop from 2000 to 2017
  if(!file.exists(here::here("public_data","Exposure",paste0(s_c_table[num,"FIPS"],"_",year,".RData")))){
    ###Load basic data
    zip_shps<-list.files(here::here("public_data","Zipcode_Polygon","polygon"),pattern = ".shp")
    zip_shps<-zip_shps[1:36%%2==1]
    zip_years<-as.numeric(paste0("20",substr(zip_shps,5,6)))
    zip_files<-cbind.data.frame(zip_years,zip_shps)
    
    post_csvs<-list.files(here::here("public_data","Zipcode_Polygon","pobox_csv"))
    post_years<-as.numeric(paste0("20",substr(post_csvs,5,6)))
    post_files<-cbind.data.frame(post_years,post_csvs)
    
    pdm_tiffs<-list.files(here::here("public_data","Population Density"),pattern = ".tif")
    pdm_years<-as.numeric(substr(pdm_tiffs,65,68))
    pdm_files<-cbind.data.frame(pdm_years,pdm_tiffs)
    
    ###create a folder to save temporary dataset
    if(!dir.exists(temp_path)){dir.create(temp_path)}
    fips_exposure=list()
    f=1
    interval=interval(start = as.Date(paste0(year,"-01-01")),end = as.Date(paste0(year,"-12-31")))
    if(!file.exists(paste0(temp_path,year,"_Wells.RData"))){
      load(here::here("public_data","Wells","Wells_4th.RData"))
      coordinates(wells)<-~lon+lat
      proj4string(wells)<-geoprjstring
      wells<-spTransform(wells,prjstring)
      wells$Construct_Interval=interval(wells$SpudDate,wells$CompletionDate)
      wells$Active_Interval=interval(wells$CompletionDate,wells$EndDate)
      all_wells=wells[!is.na(wells$Active_Interval),]
      act_wells<-all_wells[lubridate::int_overlaps(all_wells$Active_Interval,interval),]
      cons_wells<-all_wells[lubridate::int_overlaps(all_wells$Construct_Interval,interval),]
      save(file=paste0(temp_path,year,"_Wells.RData"),act_wells,cons_wells)
    }else{
      load(paste0(temp_path,year,"_Wells.RData"))
    }
    print(paste(Sys.time(),year,"Load Wells Completed"))
    
    ###calculate the annually mean wind field
    if(!file.exists(paste0(temp_path,year,"_Wind.RData"))){
      uwind<-stack(here::here("public_data","Metero","uwnd.10m.mon.mean.nc"))
      vwind<-stack(here::here("public_data","Metero","vwnd.10m.mon.mean.nc"))
      annual_uwnd<-uwind[[grep(names(uwind),pattern=year)]]
      annual_vwnd<-vwind[[grep(names(vwind),pattern=year)]]
      mean_u=calc(annual_uwnd,fun=mean)
      mean_v=calc(annual_vwnd,fun=mean)
      save(file =paste0(temp_path,year,"_Wind.RData"),mean_u,mean_v)
    }else{
      load(paste0(temp_path,year,"_Wind.RData"))
    }
    print(paste(Sys.time(),year,"Load Wind Completed"))
    if(!file.exists(paste0(temp_path,"/",year,"_Shapes.RData"))){
      zips=shapefile(here::here("public_data","Zipcode_Polygon","polygon",zip_files[which.min(abs(zip_files$zip_years-year)),"zip_shps"]))
      posts=read.csv(here::here("public_data","Zipcode_Polygon","pobox_csv",post_files[which.min(abs(post_files$post_years-year)),"post_csvs"]))
      pdm_us=raster(here::here("public_data","Population Density",pdm_files[which.min(abs(pdm_files$pdm_years-year)),"pdm_tiffs"]))
      pdm_us<-crop(pdm_us,states)
      save(file=paste0(temp_path,year,"_Shapes.RData"),zips,posts,pdm_us)
    }else{
      load(paste0(temp_path,year,"_Shapes.RData"))
    }
    print(paste(Sys.time(),year,"Load Shapes Completed"))
    if(file.exists(paste0(temp_path,year,"_PDM.RData"))){
      load(paste0(temp_path,year,"_PDM.RData"))
    }else{
      pdm_us_sp=projectRaster(pdm_us,crs=prjstring,res=1000,method="ngb")
      save(file=paste0(temp_path,year,"_PDM.RData"),pdm_us_sp) 
    }
    
    act_v_wells<-act_wells[act_wells$DrillType=="V",]
    act_h_wells<-act_wells[act_wells$DrillType=="H",]
    
    cons_v_wells<-cons_wells[cons_wells$DrillType=="V",]
    cons_h_wells<-cons_wells[cons_wells$DrillType=="H",]
    for(zip in FIPS_ZIPCODE_TABLE[FIPS_ZIPCODE_TABLE$fips==s_c_table[num,"FIPS"],"zips"]){
      if(zip%in%zips$ZIP){
        zip_polygon=zips[zips$ZIP==zip,]
        zip_sp=spTransform(zip_polygon,prjstring)
        zip_sp_bf<-buffer(zip_sp,10000) 
      }else if(zip%in%posts$ZIP){
        zip_center=posts[posts$ZIP==zip,]
        coordinates(zip_center)<-~POINT_X+POINT_Y
        proj4string(zip_center)=geoprjstring
        zip_center=spTransform(zip_center,prjstring)
        zip_sp_bf<-buffer(zip_center,5000)
        zip_sp=zip_sp_bf
        zip_sp_bf<-buffer(zip_center,10000)
        zip_polygon=spTransform(zip_sp,geoprjstring)
      }else{
        print(paste("No Shape for Zipcode",zip))
        next
      }
      
      ###exclude pixels out of the zipcode
      pdm_zip=crop(pdm_us,zip_polygon)
      names(pdm_zip)="density"
      pdm_sp=projectRaster(pdm_zip,crs=prjstring,res=1000,method="ngb")
      zip_mean_u<-projectRaster(from=mean_u,to=pdm_sp,method = "bilinear")
      zip_mean_v<-projectRaster(from=mean_v,to=pdm_sp,method="bilinear")
      
      pdm_zip=mask(pdm_sp,zip_sp)
      pdm_points=rasterToPoints(pdm_zip,spatial = T)
      if(nrow(pdm_points)==0){
        next
      }else{
        pdm_points_sp=spTransform(pdm_points,prjstring)
        a_v_wells=crop(act_v_wells,zip_sp_bf)
        c_v_wells=crop(cons_v_wells,zip_sp_bf)
        a_h_wells=crop(act_h_wells,zip_sp_bf)
        c_h_wells=crop(cons_h_wells,zip_sp_bf)
        
        res_points<-st_as_sf(pdm_points_sp)
        res_points$ID=1:nrow(res_points)
        
        ####values about the active horizontal wells
        a_h<-area_density(res=res_points,points=a_h_wells,uwnd=zip_mean_u,vwnd = zip_mean_v,radius = 15000)
        c_h<-area_density(res=res_points,points=c_h_wells,uwnd=zip_mean_u,vwnd = zip_mean_v,radius = 15000)
        a_v<-area_density(res=res_points,points=a_v_wells,uwnd =zip_mean_u,vwnd=zip_mean_v,radius=15000)
        c_v<-area_density(res=res_points,points=c_v_wells,uwnd=zip_mean_u,vwnd = zip_mean_v,radius = 15000)
        t<-bind_rows(a_h,c_h,a_v,c_v)
        t<-as.data.frame(t)
        t$vars=c("a_h","c_h","a_v","c_v")
        t$zipcode=zip
        t$year=year
        t$fips=s_c_table[num,"FIPS"]
        print(Sys.time())
        fips_exposure[[f]]=(t)
        f=f+1 
      }
    }
    fips_exposure<-do.call(rbind,fips_exposure)
    fips_exposure<-as.data.frame(fips_exposure)
    save(file=here::here("public_data","Exposure",paste0(s_c_table[num,"FIPS"],"_",year,".RData")),fips_exposure)
  }else{
    print(paste0(here::here("public_data","Exposure",paste0(s_c_table[num,"FIPS"],"_",year,".RData"))," Existed"))
  }
}
