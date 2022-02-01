# multi-temporal analysis in vegetation state in Madagascar 

library(ncdf4) # for uploading and visualizing copernicus data in R
library(raster)  #to manage spatial data, read and manipulate them
library(RStoolbox) #for remote sensing image processing
library(viridis) #brings to R color scales designed to improve graph readability
library(ggplot2) # create graphics and plotting with aestethics
library(patchwork) #to combine multiple ggplot2 plots in a plot composition
library(gridExtra) #for grid.arrange plotting, creating a multiframe 


#first let's set the working directory

setwd("C:/Users/franc/Desktop/lab/exam")


#NDVI 
# create the list of files stored in exam folder starting from a pattern "NDVI300" using list.files function

list.NDVI <- list.files(pattern="NDVI300")

#using lapply function to apply the raster function over a list

r.list <- lapply(list.NDVI, raster)
r.list

#rename the files with new simple names
n.2014 <- r.list[[1]]
n.2020 <- r.list[[2]]

#cropping on Madagascar 

ext <- extent(41, 52, -28, -10)
c.2014 <- crop(n.2014, ext)
c.2020 <- crop(n.2020, ext)

# choosing a colorRampPalette and plotting
NC <- colorRampPalette(c("chocolate4", "orange", "yellow", "grey", "green", "forestgreen", "darkgreen"))(100)

# I tried also with these colours choice but was not so good
cl <- colorRampPalette(c('darkblue', 'deepskyblue4', 'cyan3', 'pink', 'gold', 'white'))(100)
cl <- colorRampPalette(c('darkblue', 'cyan3', 'pink', 'gold', 'white'))(100)


plot(c.2014, col=NC, main="NDVI 2014")
plot(c.2020, col=NC, main="NDVI 2020")

#let's see the differences in NDVI between 2020 and 2014
NDVIdif <- c.2020 - c.2014
plot(NDVIdif, col=NC, main="Difference in NDVI between 2020 and 2014")


############Top Of Canapy 

#create the list of files stored in exam folder starting from a pattern "TOCR" using list.files function

import <- list.files(pattern="TOCR")
import 

#create a raster file using lapply function to apply raster function to the list

rast_list <- lapply(import, raster)
rast_list

# renomanate both raster 
r1999 <- rast_list[[1]]
r2014 <- rast_list[[2]]

#crop both files to see only Madagascar establishing the extention 

ext <- extent(41, 52, -28, -10)
c1999 <- crop(r1999, ext)
c2014 <- crop(r2014, ext)


#create a stack file 
sTOC <- stack(c2014, c1999)
plot(sTOC, col=NC)
NC <- colorRampPalette(c("chocolate4", "orange", "yellow", "grey", "green", "forestgreen", "darkgreen"))(100)

#histogram
hist(sTOC, col="limegreen", xlab="TOC change", main="TOC histogram")


#calculating Difference of TOC from stack differencing two images of energy in two different times

diff <- sTOC$Normalized.Top.Of.Canopy.Reflectance.in.Blue.band.1 - sTOC$Normalized.Top.Of.Canopy.Reflectance.in.Blue.band.2

#changing the palette and using HEX codes and plotting
CLF2 <- colorRampPalette(c("#B2182B","#D6604D","#F4A582","#FDDBC7","#D1E5F0","#92C5DE","#4393C3","#2166AC"))(100)

plot(diff, col=CLF2, main="Difference in TOC between 2014 and 1999")

#histogram
hist(diff, col="limegreen", xlab="TOC distribution", main="TOC histogram")


############Leaf Area Index

#I want to deepen the analysis using LAI1km data. 
#make a list of the data 

rlist <- list.files(pattern="LAI")

#choosing the extention of Madagascar 
ext <- extent(41, 52, -28, -10)

#importing the file creating a raster file
lai_list  <- lapply(rlist, raster)

#rename the file and cropping it
l1999 <- lai_list[[2]]
lai1999 <- crop(l1999, ext)
lai1999

#rename the file and cropping it
l2019 <- lai_list[[1]]
lai2019 <- crop(l2019, ext)
lai2019

#make a ggplot to use aestathics mapping and viridis scale

p1 <- ggplot() + 
geom_raster(lai1999, mapping = aes(x=x, y=y, fill=Leaf.Area.Index.1km )) +
scale_fill_viridis(option="plasma") +
ggtitle("LAI 1999")

p2 <- ggplot() + 
geom_raster(lai2019, mapping = aes(x=x, y=y, fill=Leaf.Area.Index.1km )) +
scale_fill_viridis(option="plasma") +
ggtitle("LAI 2019")

#using grid.arrage function to build a multiframe
grid.arrange(p1, p2, nrow=1)


#hist function to see more in detail trough a division of LAI in classes 

hist(lai1999, col="limegreen", xlab="LAI change", main= "LAI 1999")
hist(lai2019, col="limegreen", xlab="LAI change", main= "LAI 2019")

#build a multiframe using the par function

par(mfrow=c(1,2))
hist(lai1999, col="limegreen", xlab="LAI", main= "LAI 1999")
hist(lai2019, col="limegreen", xlab="LAI", main= "LAI 2019")

#from histogram to boxplot, different way to see the distribution
LAI <- stack(lai1999, lai2019)
boxplot(LAI, vertical=T, axes=T, outline=F, col="limegreen", ylab="LAI", xlab="Year", main="LAI Boxplot")

# plot one variable on top of the other. abline function: you can choose the intercept and the slope of the line

plot(lai1999, lai2019, xlab='lai 1999', ylab='lai 2019')
abline(0,1, col="red", x=lai1999, y=lai2019)




