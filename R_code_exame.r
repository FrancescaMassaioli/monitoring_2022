# I want to analyse

library(ncdf4)
library(raster)
library(RStoolbox)
library(viridis)
library(ggplot2)
library(patchwork)

#first let's set the working directory

setwd("C:/Users/franc/Desktop/lab/EXAM")

# create a raster file 

ndvi2020 <- raster("c_gls_NDVI_202006210000_GLOBE_PROBAV_V3.0.1.nc")

#brick function to see how many layers are: to create a set of different layer of an image 

ndviveg2020_ <- brick("c_gls_NDVI_202006210000_GLOBE_PROBAV_V3.0.1.nc")

#we can decide the colour of our graph, specifying a color scheme

cl <- colorRampPalette(c('darkblue', 'yellow', 'red', 'black'))(100)

# raster of 2014

ndvi2014 <- raster("c_gls_NDVI_201406110000_GLOBE_PROBAV_V3.0.1.nc")

ndvi2014_ <- brick("c_gls_NDVI_201406110000_GLOBE_PROBAV_V3.0.1.nc")



# ggplot function: 
ggplot() + 
geom_raster(ndvi2020, mapping = aes(x=x, y=y, fill=Normalized.Difference.Vegetation.Index.1km))

#ggplot function with viridis
ggplot() + 
geom_raster(ndviveg2020, mapping = aes(x=x, y=y, fill=Normalized.Difference.Vegetation.Index.1km)) +
scale_fill_viridis(option="magma") + 
ggtitle("DVI 2020")

ggplot() + 
geom_raster(ndvi2014, mapping = aes(x=x, y=y, fill=Normalized.Difference.Vegetation.Index.1km)) +
scale_fill_viridis(option="magma") + 
ggtitle("DVI 2014")

# importing all the data together with the lapply function, first we create a list
rlist <- list.files(pattern="NDVI")
rlist

#lapply function: to apply the same function to a list, we import the data togheter for single layer 
list_rast <- lapply(rlist, raster)
list_rast

#stack function: put all the data togheter 

ndvistack <- stack(list_rast)
ndvistack

#we have 2 variables inside the stack: Normalized.Difference.Vegetation.Index.1km.1, Normalized.Difference.Vegetation.Index.1km.2 

s2020 <- ndvistack$Normalized.Difference.Vegetation.Index.1km.1
s2014 <- ndvistack$Normalized.Difference.Vegetation.Index.1km.2

#patchwork them togheter: function to combine separate ggplots into the same graphic

p1 <- ggplot() + 
geom_raster(s2020, mapping = aes(x=x, y=y, fill=Normalized.Difference.Vegetation.Index.1km.1)) +
scale_fill_viridis(option="magma") + 
ggtitle("DVI 2020")

p2 <- ggplot() + 
geom_raster(s2014, mapping = aes(x=x, y=y, fill=Normalized.Difference.Vegetation.Index.1km.2)) +
scale_fill_viridis(option="magma") + 
ggtitle("DVI 2014")


# the 2 images one above the other 

p1 / p2


#day 2 


r2014 <- raster("c_gls_NDVI300_201405010000_GLOBE_PROBAV_V1.0.1.nc")

b2014 <- brick(r2014)
# you can crop your image on a certain area: you can put the extention you want to crop

# longitude (lambda) from 0 to 20
# latitude from 30 to 50

# crop the stack to the extent of Madagascar

ext <- extent(36, 56, -32, -3)

crop2014 <- crop(b2014, ext)


