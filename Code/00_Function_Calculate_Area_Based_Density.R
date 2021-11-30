########################################################################################################
#The purpose of this function is to calculate the point density of a point-data frame with in a polygon#
#With or without a weight raster underlying the point                                                  #
#Inputs:                                                                                               #
#res: a spatial point data frame of population density, which is converted from a raster data          #
#points: a spatial point data frame containing the location of wells                                   #
#radius: the radius around each grid to calculate PE and DE                                            #
#pointID: the ID column name of res                                                                    #
#pointWt: the name of the column that contains the density(weight) information                         #
#uwnd and vwnd: two binary columns indicating whether to calculate upwind or downwind exposure         #
#Outputs                                                                                               #
#a vector containing the ZIPCODE-specific PE and DE (if selected)                                      #
########################################################################################################
#
area_density<-function(res,points,radius=15000,pointID="ID",pointWt="density",uwnd=NULL,vwnd=NULL){
  UpointID=
    rlang::sym(pointID)
  UpointWt=
    rlang::sym(pointWt)
  res_coord=st_coordinates(res)
  IDW_All=NA
  IDW_up=NA
  IDW2_All=NA
  IDW2_up=NA
  IDW_down=NA
  IDW2_down=NA
  if(is.null(points)){
    #if there's no wells within the extent, no need to waste time
    print("No wells")
    }else{
    #seperate whehter only calculate upwind wells
    #if no wind field is provided, calculate all direction
    res_bf<-st_buffer(x=res,dist=radius,nQuadSegs=10)
    res_bf=res_bf%>%dplyr::select(ID)
    lst<-lapply(1:nrow(res_bf),function(i){
      t<-st_intersection(res_bf[i,],st_as_sf(points))
      st_crs(t)<-st_crs(res)
      if(nrow(t)>0){
        t=st_distance(t,res[i,])
        out<-data.frame(sum(1000/t),sum((1000/t)^2))
        names(out)<-c("All","All2")
      }else{
        out<-data.frame(NA,NA)
        names(out)<-c("All","All2")
      }
      out
    })
    lst<-bind_rows(lst)
    res$All=lst$All
    res[is.na(res$All),"All"]=0
    IDW_All=weighted.mean(x=res$All,w=as.data.frame(res)[,pointWt],na.rm=T)
    res$All2=lst$All2
    res[is.na(res$All2),"All2"]=0
    IDW2_All=weighted.mean(x=res$All2,w=as.data.frame(res)[,pointWt],na.rm=T)
    if(!is.null(uwnd)){
      #get the unwind wells for each grid cell, then calculate distance
      
      #Get the local wind of each grid points
      res_wind<-prepare_wind_field(as(res, "Spatial"),uwnd,vwnd,key="ID")
      #select the wells within 10 km.
      res_wind$u=res_wind$uwind*2*radius/res_wind$vel
      res_wind$v=res_wind$vwind*2*radius/res_wind$vel
      #create upwind triangles with a side of 30km
      #1st counerclosewise rotate 45 degree
      res_wind$u1=cos(-pi/4)*res_wind$u-sin(-pi/4)*res_wind$v
      res_wind$v1=sin(-pi/4)*res_wind$u+cos(-pi/4)*res_wind$v
      #then closewise rotate 45 degree
      res_wind$u2=cos(pi/4)*res_wind$u-sin(pi/4)*res_wind$v
      res_wind$v2=sin(pi/4)*res_wind$u+cos(pi/4)*res_wind$v
      #get the nodes of triangle
      res_wind[,c("x","y")]=st_coordinates(res_points)
      #create upwind triangle##################
      res_wind$x1=res_wind$x-res_wind$u1
      res_wind$y1=res_wind$y-res_wind$v1
      res_wind$x2=res_wind$x-res_wind$u2
      res_wind$y2=res_wind$y-res_wind$v2
      #create equal-side triangles with the nodes
      lst <- lapply(1:nrow(res_wind), function(i){
        ## create a matrix of coordinates that also 'close' the polygon
        res <- matrix(c(res_wind[i, 'x'], res_wind[i, 'y'],
                        res_wind[i, 'x1'], res_wind[i, 'y1'],
                        res_wind[i, 'x2'], res_wind[i, 'y2'],
                        res_wind[i, 'x'], res_wind[i, 'y'])  ## need to close the polygon
                      , ncol =2, byrow = T
        )
        ## create polygon objects
        st_polygon(list(res))
      })
      trigs <- st_sf(st_sfc(lst))
      st_crs(trigs)<-st_crs(res)
      #create a circular buffer, and calculate upwind distance
      lst<-lapply(1:nrow(res_wind),function(i){
        #create the circular section buffer
        t<-st_intersection(trigs[i,],res_bf[i,])
        #select the categorical wells within the buffer
        t<-st_intersection(t,st_as_sf(points))
        #calculate the straight-line distance between center and wells
        st_crs(t)<-st_crs(res)
        if(nrow(t)>0){
          t=st_distance(t,res[i,])
          out<-data.frame(sum(1000/t),sum((1000/t)^2))
          names(out)<-c("Up","Up2")
        }else{
          out<-data.frame(NA,NA)
          names(out)<-c("Up","Up2")
        }
        out
      })
      lst<-bind_rows(lst)
      res$Up=lst$Up
      res[is.na(res$Up),"Up"]=0
      IDW_up=weighted.mean(x=res$Up,w=as.data.frame(res)[,pointWt],na.rm=T)
      res$Up2=lst$Up2
      res[is.na(res$Up2),"Up2"]=0
      IDW2_up=weighted.mean(x=res$Up2,w=as.data.frame(res)[,pointWt],na.rm=T)
      
      #create downwind triange;
      res_wind$x1=res_wind$x+res_wind$u1
      res_wind$y1=res_wind$y+res_wind$v1
      res_wind$x2=res_wind$x+res_wind$u2
      res_wind$y2=res_wind$y+res_wind$v2
      lst <- lapply(1:nrow(res_wind), function(i){
        ## create a matrix of coordinates that also 'close' the polygon
        res <- matrix(c(res_wind[i, 'x'], res_wind[i, 'y'],
                        res_wind[i, 'x1'], res_wind[i, 'y1'],
                        res_wind[i, 'x2'], res_wind[i, 'y2'],
                        res_wind[i, 'x'], res_wind[i, 'y'])  ## need to close the polygon
                      , ncol =2, byrow = T
        )
        ## create polygon objects
        st_polygon(list(res))
      })
      trigs <- st_sf(st_sfc(lst))
      st_crs(trigs)<-st_crs(res)
      #create a circular buffer, and calculate upwind distance
      lst<-lapply(1:nrow(res_wind),function(i){
        #create the circular section buffer
        t<-st_intersection(trigs[i,],res_bf[i,])
        #select the categorical wells within the buffer
        t<-st_intersection(t,st_as_sf(points))
        #calculate the straight-line distance between center and wells
        st_crs(t)<-st_crs(res)
        if(nrow(t)>0){
          t=st_distance(t,res[i,])
          out<-data.frame(sum(1000/t),sum((1000/t)^2))
          names(out)<-c("Down","Down2")
        }else{
          out<-data.frame(NA,NA)
          names(out)<-c("Down","Down2")
        }
        out
      })
      lst<-bind_rows(lst)
      res$Down=lst$Down
      res[is.na(res$Down),"Down"]=0
      IDW_down=weighted.mean(x=res$Down,w=as.data.frame(res)[,pointWt],na.rm=T)
      res$Down2=lst$Down2
      res[is.na(res$Down2),"Down2"]=0
      IDW2_down=weighted.mean(x=res$Down2,w=as.data.frame(res)[,pointWt],na.rm=T)
    }
  }
  output=c(IDW_All,IDW2_All,IDW_up,IDW2_up,IDW_down,IDW2_down)
  names(output)=c("Inv_1st","Inv_2nd","Inv_1st_Up","Inv_2nd_Up","Inv_1st_Down","Inv_2nd_Down")
  return(output)
}
