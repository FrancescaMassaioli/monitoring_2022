# GREENLAND 

# R_code_greenland.r

# Time series analysis
# Greenland increase of temperature
# Data and code from Emanuela Cosma

# install.packages("raster")
# install.packages("rasterVis")
library(raster)
#library(rasterVis) 
library(patchwork)
# library(rgdal)

setwd("~/lab/greenland") # Linux
# setwd("C:/lab/greenland") # Windows
# setwd("/Users/name/Desktop/lab/greenland") # Mac 

# list f files:we create the list of all files stored in R starting from a pattern
rlist <- list.files(pattern="lst")
rlist

# apply the raster funciton as they are single layers, import data. lapply function to apply the raster function to the list

import <- lapply(rlist,raster)
import

# let's make a stack to take them togheter

tgr <- stack(import)
tgr

plot(TGr)

#levelplot(TGr)
cl <- colorRampPalette(c("blue","light blue","pink","yellow"))(100)
plot(tgr, col=cl)

# ggplot of first and final 2000 vs 2015
p1 <- ggplot() + 
geom_raster(tgr$lst_2000, mapping = aes(x=x, y=y, fill=lst_2000)) +
scale_fill_viridis(option="magma") 

p2 <- ggplot() + 
geom_raster(tgr$lst_2015, mapping = aes(x=x, y=y, fill=lst_2015)) +
scale_fill_viridis(option="magma") 




plot(TGr,col=cl, names.attr=c("July 2000","July 2005", "July 2010", "July 2015"))

plot(TGr,col.regions=cl, main="LST variation in time", names.attr=c("July 2000","July 2005", "July 2010", "July 2015"))

p1 <- ggplot() + 
geom_raster(TGr$lst_2000, mapping = aes(x=x, y=y, fill=lst_2000)) +
scale_fill_viridis(option="magma") 
# ggtitle("cividis palette")

p2 <- ggplot() + 
geom_raster(TGr$lst_2015, mapping = aes(x=x, y=y, fill=lst_2015)) +
scale_fill_viridis(option="magma")

# plot them togheter 
p1+p2

# how distributions is changing, histogram . 
 hist(tgr$lst_2000)
 
# plotting frequency distributions of data 
par(mfrow=c(1, 2))
hist(tgr$lst_2000)
hist(tgr$lst_2005)
hist(tgr$lst_2010)
hist(tgr$lst_2015)

# plot one variable on top of the other. abline function: you can choose the intercept and the slope of the line

plot(tgr$lst_2010, tgr$lst_2015)
abline(0,1,col="red")

# to put the line starting from zero, xlim
plot(tgr$lst_2010, tgr$lst_2015, xlim=c(12500,15000), ylim=c(12500, 15000))
abline(0,1,col="red")

# make a plot with all the histograms and all the regressions for all the values
par(mfrow=c(4, 4))
hist(tgr$lst_2000)
hist(tgr$lst_2005)
hist(tgr$lst_2010)
hist(tgr$lst_2015)
plot(tgr$lst_2010, tgr$lst_2015, xlim=c(12500,15000), ylim=c(12500, 15000))
plot(tgr$lst_2010, tgr$lst_2000, xlim=c(12500,15000), ylim=c(12500, 15000))
plot(tgr$lst_2010, tgr$lst_2005, xlim=c(12500,15000), ylim=c(12500, 15000))
plot(tgr$lst_2000, tgr$lst_2015, xlim=c(12500,15000), ylim=c(12500, 15000))
plot(tgr$lst_2000, tgr$lst_2005, xlim=c(12500,15000), ylim=c(12500, 15000))
plot(tgr$lst_2015, tgr$lst_2005, xlim=c(12500,15000), ylim=c(12500, 15000))

# other solutions is using a stack: pairs function. scatterplot matrix

pairs(tgr)

