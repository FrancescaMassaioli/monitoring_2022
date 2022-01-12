# R code for uploading and visualizing Coprnicus data in R

# install.packages("ncdf4")
# install.packages("viridis")
library(ncdf4)
library(raster)
library(RStoolbox)
library(viridis)
library(ggplot2)
library(patchwork)

# Set the working directory
setwd("~/lab/copernicus/") # Linux 
# setwd("C:/lab//copernicus/")  # windows
# setwd("/Users/name/lab/copernicus/") # mac

snow20211214 <- raster("c_gls_SCE_202112140000_NHEMI_VIIRS_V1.0.1.nc")
# to see how many layers are inside Copernicus data:
# snow20211214_ <- brick("c_gls_SCE_202112140000_NHEMI_VIIRS_V1.0.1.nc")

snow20211214

plot(snow20211214)

cl <- colorRampPalette(c("dark blue","blue","light blue"))(100)
plot(snow20211214, col=cl)

# this is bad!
cl <- colorRampPalette(c("blue","green","red"))(100)
plot(snow20211214, col=cl)

#########à
# ggplot function
ggplot() + 
geom_raster(snow20211214, mapping = aes(x=x, y=y, fill=Snow.Cover.Extent))  

# ggplot function with viridis
ggplot() + 
geom_raster(snow20211214, mapping = aes(x=x, y=y, fill=Snow.Cover.Extent)) +
scale_fill_viridis() 

ggplot() + 
geom_raster(snow20211214, mapping = aes(x=x, y=y, fill=Snow.Cover.Extent)) +
scale_fill_viridis(option="cividis") 

ggplot() + 
geom_raster(snow20211214, mapping = aes(x=x, y=y, fill=Snow.Cover.Extent)) +
scale_fill_viridis(option="cividis") + 
ggtitle("cividis palette")


#################################################### day 2

# importing all the data together with the lapply function
rlist <- list.files(pattern="SCE500")
rlist

# we import the data for single layer

list_rast <- lapply(rlist, raster)
list_rast

# stack: all the data togheter, at the moment they are separated.

snowstack <- stack(list_rast)
snowstack

# we are going to use 2 variable -> link the variable to a new name

s2017 <- snowstack$Snow.Cover.Extent.1
s2022 <- snowstack$Snow.Cover.Extent.2

# patchwork to plot all togheter 
# ggplot : you open the window and you add something

ggplot() + 
geom_raster(s2017, mapping = aes(x=x, y=y, fill=Snow.Cover.Extent.1)) +

# viridis : option to have different legend palette

scale_fill_viridis(option="viridis") +
ggtitle("Snow cover during years!")

# 

ggplot() + 
geom_raster(s2022, mapping = aes(x=x, y=y, fill=Snow.Cover.Extent.2)) +
scale_fill_viridis(option="viridis") +
ggtitle("Snow cover during freezing winter!")

# let's patchwork them together

p1 <- ggplot() + 
geom_raster(s2017, mapping = aes(x=x, y=y, fill=Snow.Cover.Extent.1)) +
scale_fill_viridis(option="viridis") +
ggtitle("Snow cover during years!")

p2 <- ggplot() + 
geom_raster(s2022, mapping = aes(x=x, y=y, fill=Snow.Cover.Extent.2)) +
scale_fill_viridis(option="viridis") +
ggtitle("Snow cover during freezing winter!")

# the 2 images one above the other 

p1 / p2


# you can crop your image on a certain area: you can put the extention you want to crop

# longitude (lambda) from 0 to 20
# latitude from 30 to 50

# crop the stack to the extent of Sicily
ext <- c(0, 20, 30, 50)

# stack_cropped <- crop(snowstack, ext) # this will crop the whole stack, and then single variables (layers) can be extracted

s2017_cropped <- crop(s2017, ext)
s2022_cropped <- crop(s2022, ext)

plot(ssummer_cropped)


p1 <- ggplot() + 
geom_raster(ssummer_cropped, mapping = aes(x=x, y=y, fill=Snow.Cover.Extent.1)) +
scale_fill_viridis(option="viridis") +
ggtitle("Snow cover during my birthday!")

p2 <- ggplot() + 
geom_raster(swinter_cropped, mapping = aes(x=x, y=y, fill=Snow.Cover.Extent.2)) +
scale_fill_viridis(option="viridis") +
ggtitle("Snow cover during freezing winter!")

p1 / p2
