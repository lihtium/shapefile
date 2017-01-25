#=============================
# Luxembourg Spatial Analysis
#=============================

install.packages("sp")
install.packages("fastshp")
install.packages("rgdal")
install.packages("maptools")
install.packages("RgoogleMaps")

library(sp)
library(rgdal)
library(ggplot2)
library(RColorBrewer)

# The data can be freely downloaded through the luxembourgish opendata website
address<-read.csv("./data/addresses.csv",sep=";",dec=".")
# readOGR reads shapefiles
communes<-readOGR("./data/LIMADM_COMMUNES.shp",layer="LIMADM_COMMUNES")

# readOGR also reads the projection system
proj4string(communes)
# the result should be : "+proj=tmerc +lat_0=49.83333333333334 +lon_0=6.166666666666667 +k=1 +x_0=80000 +y_0=100000 +ellps=intl +units=m +no_defs"

# address won't have a projection system
proj4string(address)
# This is normal because address is a normal data.frame
# Let's convert it into a spatial points data frame
names(address)
# The longitude and latitude is labeled lon_wgs84 and lat_wgs84
coordinates(address)<-~lon_wgs84+lat_wgs84
class(address)
# This will not give address a proper projection system, but at least we can affect one now
proj4string(address)
# This sould work
proj4string(address)<-CRS("+proj=longlat +datum=WGS84")

# In order to project address and communes into one single plot, we have to project the data with the same projection system
address<-spTransform(address, 
                     CRS(proj4string(communes)))
identical(proj4string(address),proj4string(communes))

plot_1<-ggplot()+geom_polygon(data=communes,
                              aes(x=long,
                                  y=lat,
                                  group=group),
                              fill="grey",
                              color="black")
                              
plot_2<-plot_1+geom_point(data=address,
                          aes(x=lon_wgs84,
                              y=lat_wgs84),
                          color="red")
# You will see that ggplot2 doesn't know how to read spatial data frames
address<-data.frame(address)
plot_2<-plot_1+geom_point(data=address,
                          aes(x=lon_wgs84,
                              y=lat_wgs84
                              ),
                          color="red",
                          size=0.5)+
  coord_equal(ratio=1)
# Plot the map
plot_2

# Let's assume that we have 2 group of houses A and B
address$class<-sample(c("A","B"),length(address$rue),replace=TRUE)
# This could be for example appartments vs houses, clients vs potential clients etc.

plot_3<-plot_1+geom_point(data=address,
                          aes(x=lon_wgs84,
                              y=lat_wgs84,
                              color=class
                          ),
                          size=0.5)+
  coord_equal(ratio=1)
plot_3


