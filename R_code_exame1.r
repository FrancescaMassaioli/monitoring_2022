# I want to analyse changes in vegetation state in Madagascar

library(ncdf4) 
library(raster)
library(RStoolbox)
library(viridis)
library(ggplot2)
library(patchwork)
library(rgdal)


#first let's set the working directory

setwd("C:/Users/franc/Desktop/lab/exam")

# import the data from lab folder to R using list.files function. 

import <- list.files(pattern="TOCR")
import 

# create a raster file (using lapply function to apply a function to a list)

rast_list <- lapply(import, raster)
rast_list

# renomanate both raster 
r1999 <- rast_list[[1]]

#crop the file to see only Madagascar 

ext <- extent(41, 52, -28, -10)
c1999 <- crop(r1999, ext)

r2018 <- rast_list[[2]]
c2018 <- crop(r2018, ext)

# plotting the images with a color choice

cl <- colorRampPalette(c('darkblue', 'yellow', 'red', 'black'))(100)

plot_1999 <- plot(c1999, col=cl)

plot_2018 <- plot(c2018, col=cl)

  
#ggplot function with viridis
ggplot() + 
geom_raster(c1999, mapping = aes(x=x, y=y, fill=Normalized.Top.Of.Canopy.Reflectance.in.Blue.band)) +
scale_fill_viridis_c(option = "magma")
ggtitle("TOC 1999")

ggplot() + 
geom_raster(c2018, mapping = aes(x=x, y=y, fill=Normalized.Top.Of.Canopy.Reflectance.in.Blue.band)) +
scale_fill_viridis(option="magma") + 
ggtitle("TOC 2018")


#import image


list <- list.files(pattern="plot_")
list
# [1] "plot_1999raster.jpeg" "plot_2018raster.jpeg"

#create a brick raster file to see all the layers

a1999 <- brick("plot_1999raster.jpeg")

a2018 <- brick("plot_2018raster.jpeg")
?????????????????????????'
#plotRGB NO

plotRGB(a1999, r=1, g=2, b=3, stretch="Lin") ?????????????????

plotRGB(a2018, r=2, g=3, b=1, stretch="Lin") ??????????????????????
????????????????????????????????????


# create a stack to plot all layers at the same time
s1999 <- stack(a1999)
plot(s1999, col=cl)

s2018 <- stack(a2018)
plot(s2018, col=cl)

# calculate energy in 1999

dvi1999 <- s1999$plot_1999raster.1 - s1999$plot_1999raster.2 / s1999$plot_1999raster.1 + s1999$plot_1999raster.2
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100)
plot(dvi1999, col=cl)

dvi2018 <- s2018$plot_2018raster.1 - s2018$plot_2018raster.2 / s2018$plot_2018raster.1 + s2018$plot_2018raster.2
plot(dvi2018, col=cl)

# differencing two images of energy in two different times

dvidif <- dvi1999 - dvi2018
cld <- colorRampPalette(c('blue','white','red'))(100)

# result: big difference of energy. 

par(mfrow=c(2,2))
plot(dvi1999, col=cl)
plot(dvi2018, col=cl)
plot(dvidif, col=cld)

#LAI1km 

rlist <- list.files(pattern="LAI")

ext <- extent(41, 52, -28, -10)

lai_list  <- lapply(rlist, raster)

l1999 <- lai_list[[2]]
lai1999 <- crop(l1999, ext)
lai1999

l2019 <- lai_list[[1]]
lai2019 <- crop(l2019, ext)
lai2019

cl <- colorRampPalette(c("blue","light blue","pink","yellow"))(100) (no è quella di prima ora)

plot(lai1999, col=cl)
dev.off()
plot(lai2019, col=cl)

par(mfrow=c(2,1))
plot(lai1999, col=cl)
plot(lai2019, col=cl)

#ggplot

p1 <- ggplot() + 
geom_raster(lai1999, mapping = aes(x=x, y=y, fill=Leaf.Area.Index.1km )) +
scale_fill_viridis(option="plasma") +
ggtitle("LAI 1999")

p2 <- ggplot() + 
geom_raster(lai2019, mapping = aes(x=x, y=y, fill=Leaf.Area.Index.1km )) +
scale_fill_viridis(option="plasma") +
ggtitle("LAI 2019")


#hist function

hist(lai1999)
hist(lai2019)

par(mfrow=c(1,2))
hist(lai1999)
hist(lai2019)

# plot one variable on top of the other. abline function: you can choose the intercept and the slope of the line
plot(lai1999, lai2019)
abline(0,1,col="red")

#unsupervised classification --> to classify what are the forest and what baresoil area. we will use the Rtoolbox to make classification
# unsuperClass(img, nClasses)
lai_mad <- list.files(pattern="LAI_")

list_ <- lapply(lai_mad, brick)
l1999b <- list_[[1]]

l2019b <- list_mad[[2]]


l1999c <- unsuperClass(l1999b, nClasses=2)
l1999c
plot(l1999c$map) 

l2019c <- unsuperClass(l2019b, nClasses=2)
l2019c
plot(l2019c$map)

#frequencies
freq(l1999c$map)

# value  count
#[1,]     1 241293
#[2,]     2  20339

total <- 770560
propforest <- 751693/total
propbaresoil <- 18867/total

# propforest 0.922261
# propbaresoil 0.07773896

