# multi-temporal analysis in vegetation state in Madagascar 

library(ncdf4) # for uploading and visualizing copernicus data in R
library(raster)  #to manage satellite data 
library(RStoolbox) #classification
library(viridis) #brings to R color scales designed to improve graph readability
library(ggplot2) # plotting with aestethics
library(patchwork) #to combine multiple ggplot2 plots in a plot composition
library(gridExtra) #for grid.arrange plotting, creating a multiframe 


#first let's set the working directory

setwd("C:/Users/franc/Desktop/lab/exam")


#NDVI 
# create the list of files stored in exam folder starting from a pattern "NDVI300" using list.files function

list.NDVI <- list.files(pattern="NDVI300")

r.list <- lapply(list.NDVI, raster)
r.list

n.2014 <- r.list[[1]]
n.2020 <- r.list[[2]]


c.2014 <- crop(n.2014, ext)
c.2020 <- crop(n.2020, ext)

NC <- colorRampPalette(c("chocolate4", "orange", "yellow", "grey", "green", "forestgreen", "darkgreen"))(100)
plot(c.2014, col=NC, main="NDVI 2014")
plot(c.2020, col=NC, main="NDVI 2020")

NDVIdif <- c.2020 - c.2014
plot(NDVIdif, col=NC, main="Difference NDVI between 2014 and 2020")


#create a stack
NDVIs <- stack(c.2014, c.2020)
plot(NDVIs, col=cl)


#boxplot of the stack
boxplot(NDVIs, vertical=T, axes=T, outline=F, col="limegreen", ylab="NDVI", xlab="Year", main="NDVI")


############################################

# create the list of files stored in exam folder starting from a pattern "TOCR" using list.files function

import <- list.files(pattern="TOCR")
import 

# create a raster file using lapply function to apply raster function to the list

rast_list <- lapply(import, raster)
rast_list

# renomanate both raster 
r1999 <- rast_list[[1]]

#crop both files to see only Madagascar 

ext <- extent(41, 52, -28, -10)
c1999 <- crop(r1999, ext)

r2014 <- rast_list[[2]]
c2014 <- crop(r2014, ext)

# plotting the images with a color choice

cl <- colorRampPalette(c('darkblue', 'deepskyblue4', 'cyan3', 'pink', 'gold', 'white'))(100)
cl <- colorRampPalette(c('darkblue', 'cyan3', 'pink', 'gold', 'white'))(100)
plot(c1999, col=cl, main="TOC 1999")

plot(c2014, col=cl, main="TOC 2014")

  
#ggplot function with viridis
ggplot() + 
geom_raster(b1999, mapping = aes(x=x, y=y, fill=Normalized.Top.Of.Canopy.Reflectance.in.Blue.band)) +
scale_fill_viridis(option = "C") +
ggtitle("TOC 1999")

ggplot() +
geom_raster(b2014, mapping = aes(x=x, y=y, fill=Normalized.Top.Of.Canopy.Reflectance.in.Blue.band)) +
scale_fill_viridis(option="C") + 
ggtitle("TOC 2014")


#create a stack

sTOC <- stack(c2014, c1999)
plot(sTOC, col=NC)

#histogram
hist(sTOC, col="limegreen", xlab="TOC change", main="TOC histogram")

#boxplot
boxplot(sTOC, vertical=T, axes=T, outline=F, col="limegreen", ylab="NDVI", xlab="Year", main="TOC")

#calculating Difference of TOC from stack differencing two images of energy in two different times

L1 <- sTOC$Normalized.Top.Of.Canopy.Reflectance.in.Blue.band.1 - sTOC$Normalized.Top.Of.Canopy.Reflectance.in.Blue.band.2
plot(L1, col=NC)

#histogram
hist(L1, col="limegreen", xlab="DVI", main="difference")

# fare par con hist e boxplot
par(mfrow=c(2,2))
hist(L1, col="limegreen", xlab="DVI", main="difference")
boxplot(sTOC, vertical=T, axes=T, outline=F, col="limegreen", ylab="NDVI", xlab="Year", main="TOC")



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

#plotting them with a colour choice
CLAI <- colorRampPalette(c("#1E377F","#0055CC","#0A75AD","#81D8D0","#FDD591","#FCA954","#ED7676"))(100)

plot(lai1999, col=CLAI, main="LAI 1999")
dev.off()
plot(lai2019, col=CLAI, main="LAI 2019")

#building a multiframe
par(mfrow=c(1,2))
plot(lai1999, col=CLAI, main="LAI 1999")
plot(lai2019, col=CLAI, main="LAI 2019")

par(mfrow=c(1,2))
plot(lai1999, col=NC, main="LAI 1999")
plot(lai2019, col=NC, main="LAI 2019")

#see the differences between 1999 and 2019

laidif <- lai2019 - lai1999
plot(laidif, col=CLF2, main="Difference in LAI between 1999 and 2019")
CLF2 <- colorRampPalette(c("#B2182B","#D6604D","#F4A582","#FDDBC7","#D1E5F0","#92C5DE","#4393C3","#2166AC"))(100)



#make a ggplot to use aestathics mapping and viridis scale

p1 <- ggplot() + 
geom_raster(lai1999, mapping = aes(x=x, y=y, fill=Leaf.Area.Index.1km )) +
scale_fill_viridis(option="plasma") +
ggtitle("LAI 1999")

p2 <- ggplot() + 
geom_raster(lai2019, mapping = aes(x=x, y=y, fill=Leaf.Area.Index.1km )) +
scale_fill_viridis(option="plasma") +
ggtitle("LAI 2019")


#hist function to see more in detail trough a division of LAI in classes 

hist(lai1999, col="limegreen", xlab="LAI change", main= "LAI histogram")
hist(lai2019, col="limegreen", xlab="LAI change", main= "LAI histogram")

#build a multiframe 

par(mfrow=c(1,2))
hist(lai1999, col="limegreen", xlab="LAI change", main= "LAI histogram")
hist(lai2019, col="limegreen", xlab="LAI change", main= "LAI histogram")

#from hist to boxplot, different way to see the distribution
LAI <- stack(lai1999, lai2019)
boxplot(LAI, vertical=T, axes=T, outline=F, col="limegreen", ylab="LAI", xlab="Year", main="LAI Boxplot")

# plot one variable on top of the other. abline function: you can choose the intercept and the slope of the line

plot(lai1999, lai2019, xlab='lai 1999', ylab='lai 2019')
abline(0,1, col="red", x=lai1999, y=lai2019)




