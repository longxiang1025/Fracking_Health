#######################################
#Extract the wind field stack raster data by location of points (SpatialPointsDataFrame)
#And re-format the data into a long,instead of wide, format.
#points: points of interest, in this study is the location of RadNet monitors
#u: the u-components of wind field
#v: the v-components of wind field
#key: the primary key of the points data, in this study is the city_state
#######################################
prepare_wind_field<-function(points,u,v,key){
  #check the necessary libraries
  require(lubridate)
  require(dplyr)
  require(tidyr)
  require(raster)
  #set the primary key of the table
  p_u<-as.data.frame(raster::extract(u,points))
  p_u[,key]=as.factor(points@data[,key])
  p_u<-gather(p_u,time,uwind,-key)
  p_u$time=names(u)
  p_v<-as.data.frame(raster::extract(v,points))
  p_v[,key]=as.factor(points@data[,key])
  p_v<-gather(p_v,time,vwind,-key)
  p_v$time=names(v)
  #filter the data and standardize the date
  p<-left_join(p_u,p_v)
  
  p<-p[,c(key,"uwind","vwind")]
  #calculate the wind direction and velocity
  p[,"dir"]<- (270-atan2(p[,"vwind"],p[,"uwind"])*180/pi)%%360
  p[,"vel"]<-sqrt(p[,"vwind"]^2+p[,"uwind"]^2)
  #remove the duplicate of wind of Nov
  p<-p[!duplicated(p[,c(key)]),]
  return(p)
}
