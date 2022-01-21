# I want to analyse

library(ncdf4)
library(raster)
library(RStoolbox)
library(viridis)
library(ggplot2)
library(patchwork)
library(rgdal)
 
#first let's set the working directory

setwd("C:/Users/franc/Desktop/lab/exam1")

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
# you can crop your image on a certain area: you can put the extention you want to crop

# longitude (lambda) from 0 to 20
# latitude from 30 to 50

# crop the stack to the extent of Madagascar

r2014 <- raster("c_gls_NDVI300_201405010000_GLOBE_PROBAV_V1.0.1.nc")

b2014 <- brick("c_gls_NDVI300_201405010000_GLOBE_PROBAV_V1.0.1.nc")
ext <- extent(36, 56, -32, -3)
c2014 <- crop(b2014, ext)
plot(c3014, col=cl)

r2020 <- raster("c_gls_NDVI300_202005210000_GLOBE_PROBAV_V1.0.1.nc")
b2020 <- brick("c_gls_NDVI300_202005210000_GLOBE_PROBAV_V1.0.1.nc")
ext <- extent(36, 56, -32, -3)
c2020 <- crop(b2014, ext)
plot(c2020, col=cl)

#let's put the images one beside the other with par function

par(mfrow=c(1,2))
plot(c3014, col=cl)
plot(c2020, col=cl)


#ggplot function
ggplot() +
geom_raster(c2014, mapping = aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option="cividis")
ggtitle("Forest 2014")

ggplot() +
geom_raster(c2014, mapping = aes(x=x, y=y, fill=Normalized.Difference.Vegetation.Index.333M)) +
scale_fill_viridis(option="magma")
ggtitle("Forest 2020")

# to select files from my lab folder in a quick way: list.files
#importing all the data together with the lapply function

rlist <- list.files(pattern="NDVI300_20")

# we import the data for single layer, but at the same time togheter

list_rast <- lapply(rlist, raster)
list_rast

forestack <- stack(list_rast)
forestac
######################################################################

library(rgdal)

 # importing the data ENERGY


r1999 <- raster("c_gls_ALDH_199905130000_GLOBE_VGT_V1.4.1.nc")

b1999 <- brick("c_gls_ALDH_199905130000_GLOBE_VGT_V1.4.1.nc")

ext <- extent(41, 52, -28, -10)
c1999 <- crop(b1999, ext)
plot



r2020 <- raster("c_gls_ALDH_202003240000_GLOBE_PROBAV_V1.5.1.nc")

b2020 <- brick("c_gls_ALDH_202003240000_GLOBE_PROBAV_V1.5.1.nc")

ext <- extent(41, 52, -28, -10)
c2020 <- crop(b2020, ext)
plot


#import image


list <- list.files(pattern="albedo")
list
# [1] "albedo1999_.jpeg" "albedo2020_.jpeg"
a1999 <- brick("albedo1999_.jpeg")

a2020 <- brick("albedo2020_.jpeg")

#plotRGB
plotRGB(a1999, r=2, g=3, b=1, stretch="Lin")

plotRGB(a2020, r=2, g=3, b=1, stretch="Lin")

# let's calculate energy in 1999
dvi1999 <- a1999$albedo1999_.1 - a1999$albedo1999_.2
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100)
plot(dvi1999, col=cl)

dvi2020 <- a2020$albedo2020_.1 - a2020$albedo2020_.2
plot(dvi2020, col=cl)

# differencing two images of energy in two different times

dvidif <- dvi1999 - dvi2020
cld <- colorRampPalette(c('blue','white','red'))(100)


par(mfrow=c(3,2))
plotRGB(a1999, r=2, g=3, b=1, stretch="Lin")
plotRGB(a2020, r=2, g=3, b=1, stretch="Lin")
plot(dvi1999, col=cl)
plot(dvi2020, col=cl)
plot(dvidif, col=cld)


#unsupervised classification --> to classify what are the forest and what agricultural area. we will use the Rtoolbox to make classification
# unsuperClass(img, nClasses)
rlist <- list.files(pattern="albedo")
list_rast <- lapply(rlist, brick)
a1999 <- list_rast[[1]]
plotRGB(a1999, r=2, g=3, b=1, stretch="lin")
a2020 <- list_rast[[2]]
plotRGB(a2020, r=2, g=3, b=1, stretch="lin")


a1999c <- unsuperClass(a1999, nClasses=2)
a1999c
plot(a1999c$map) 

a2020c <- unsuperClass(a2020, nClasses=2)
a2020c
plot(a2020c$map)

#frequencies
freq(a1999c$map)

# value  count
#[1,]     1 241293
#[2,]     2  20339

total <- 261632
propforest <- 241293/total
propbaresoil <- 20339/total

# propforest 0.922261
# propbaresoil 0.07773896

#now we are going to build a dataframe 

cover <- c("Forest", "Baresoil")
prop1999 <- c(0.922261, 0.07773896)
prop1999 <- c(propforest, propbaresoil)
#build a dataframe
proportion1999 <- data.frame(cover, prop1999)
proportion1999

#      cover   prop1999
# 1   Forest 0.92226104
# 2 Baresoil 0.07773896


#ggplot

ggplot(proportion1999, aes(x=cover, y=prop1999, color=cover)) + geom_bar(stat="identity", fill="white")

#a2020 proportion

freq(a2020c$map)
 #value  count
#[1,]     1  16183
#[2,]     2 245449

total <- 261632
propforest <- 16183/total
propbaresoil <- 245449/total

# forest [1] 0.06185405
#baresoil [1] 0.9381459

#now we are going to build a dataframe 

cover <- c("Forest", "Baresoil")
prop2020 <- c(0.06185405, 0.9381459)
prop2020 <- c(propforest, propbaresoil)
proportion2020 <- data.frame(cover, prop2020)
proportion2020 


# cover   prop2020
#1   Forest 0.06185405
#2 Baresoil 0.93814595


#ggplot
ggplot(proportion2020, aes(x=cover, y=prop2020, color=cover)) + geom_bar(stat="identity", fill="white")

#finale dataframe

finalproportion <- data.frame(cover, prop1999, prop2020)





#     cover   prop1999   prop2020
#1   Forest 0.92226104 0.06185405
#2 Baresoil 0.07773896 0.93814595

# now we want to plot weverything all togheter -> gridExtra packages -> we are doing multiframe by using this package 
# 1 let's assign to every ggplot a name

p1999 <- ggplot(proportion1999, aes(x=cover, y=prop1999, color=cover)) + geom_bar(stat="identity", fill="white")
p2020 <- ggplot(proportion2020, aes(x=cover, y=prop2020, color=cover)) + geom_bar(stat="identity", fill="white")

grid.arrange(p1999, p2020, nrow=1)

