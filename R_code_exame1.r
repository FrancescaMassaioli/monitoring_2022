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

cl <- colorRampPalette(c('darkblue', 'deepskyblue4', 'cyan3', 'pink', 'yellow', 'gold'))(100)

plot_1999 <- plot(c1999, col=cl, main="TOC 1999")

plot_2018 <- plot(c2018, col=cl, main="TOC 2018")

  
#ggplot function with viridis
ggplot() + 
geom_raster(c1999, mapping = aes(x=x, y=y, fill=Normalized.Top.Of.Canopy.Reflectance.in.Blue.band)) +
scale_fill_viridis(option = "C") +
ggtitle("TOC 1999")

ggplot() + 
geom_raster(c2018, mapping = aes(x=x, y=y, fill=Normalized.Top.Of.Canopy.Reflectance.in.Blue.band)) +
scale_fill_viridis(option="C") + 
ggtitle("TOC 2018")


#import image


list <- list.files(pattern="TOC_")
list
# [1] "TOC_1999.jpeg" "TOC_2018.jpeg"

#create a brick raster file to see all the layers

b_1999 <- brick("TOC_1999.jpeg")

b_2018 <- brick("TOC_2018.jpeg")

#plotRGB 

plotRGB(b_1999, r=1, g=2, b=3, stretch="Lin") 

#for curiosity try also the red band in 

plotRGB(b_1999, r=3, g=2, b=1, stretch="Lin")

#red in blue (plotRGB_3)
plotRGB(b_1999, r=1, g=3, b=2, stretch="Lin")

#RGB_4
plotRGB(b_1999, r=2, g=1, b=3, stretch="Lin")



# 2018


plotRGB(b_2018, r=1, g=2, b=3, stretch="Lin") 



# create a stack file to plot all layers at the same time
cl <- colorRampPalette(c('darkblue', 'deepskyblue4', 'cyan3', 'pink', 'yellow', 'white'))(100)

cls <- colorRampPalette(c('darkblue', 'deepskyblue4', 'cyan3', 'azure2', 'yellow', 'orange', 'chocolate1'))(100)


s1999 <- stack(b_1999)
plot(b_1999, col=cl, main="STACK 1999")

s2018 <- stack(b_2018)
plot(b_2018, col=cl, main="STACK 2018")

# calculate energy in 1999
 
dvi1999 <- s1999$TOC_1999.1 - s1999$TOC_1999.2 / s1999$TOC_1999.1 + s1999$TOC_1999.2
CLAI <- colorRampPalette(c("#1E377F","#0055CC","#0A75AD","#81D8D0","#FDD591","#FCA954","#ED7676"))(100)
plot(dvi1999, col=CLAI, main="NDVI 1999")

dvi2018 <- s2018$TOC_2018.1 - s2018$TOC_2018.2 / s2018$TOC_2018.1 + s2018$TOC_2018.2
plot(dvi2018, col=CLAI, main="NDVI 2018")

# differencing two images of energy in two different times

dvidif <- dvi1999 - dvi2018     

cld <- colorRampPalette(c('blue','white','red'))(100) or NC <- colorRampPalette(c("chocolate4", "orange", "yellow", "grey", "green", "forestgreen", "darkgreen"))(100)
plot(dvidif, col=cld, main="Difference in NDVI between 1999 and 2018")

# result: big difference of energy. 

par(mfrow=c(2,2))
plot(dvi1999, col=CLAI, main="NDVI 1999")
plot(dvi2018, col=CLAI, main="NDVI 2018")
plot(dvidif, col=cld, main="Difference in NDVI between 1999 and 2018")

# see better in a histogram

hist(dvidif, col="limegreen", xlab="DVI change", main= "DVI histogram")





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

CLAI <- colorRampPalette(c("#1E377F","#0055CC","#0A75AD","#81D8D0","#FDD591","#FCA954","#ED7676"))(100)

plot(lai1999, col=CLAI, main="LAI 1999")
dev.off()
plot(lai2019, col=CLAI, main="LAI 2019")

par(mfrow=c(1,2))
plot(lai1999, col=CLAI, main="LAI 1999")
plot(lai2019, col=CLAI, main="LAI 2019")

#see the differences

laidif <- lai2019 - lai1999
plot(laidif, col=CLF2, main="Difference in LAI between 1999 and 2019")
CLF2 <- colorRampPalette(c("#B2182B","#D6604D","#F4A582","#FDDBC7","#D1E5F0","#92C5DE","#4393C3","#2166AC"))(100)



#ggplot

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

par(mfrow=c(1,2))
hist(lai1999, col="limegreen", xlab="LAI change", main= "LAI histogram")
hist(lai2019, col="limegreen", xlab="LAI change", main= "LAI histogram")

#from hist to boxplot


LAI <- stack(lai1999, lai2019)
boxplot(LAI, vertical=T, axes=T, outline=F, col="limegreen", ylab="LAI", xlab="Year", main="LAI Boxplot")





# plot one variable on top of the other. abline function: you can choose the intercept and the slope of the line

plot(lai1999, lai2019, xlab='lai 1999', ylab='lai 2019')
abline(0,1, col="red", x=lai1999, y=lai2019)

#unsupervised classification --> to classify what are the forest and what baresoil area. we will use the Rtoolbox to make classification
# unsuperClass(img, nClasses)

lai_mad <- list.files(pattern="LAIT")

list_ <- lapply(lai_mad, brick)
l1999b <- list_[[1]]

l2019b <- list_[[2]]


l1999c <- unsuperClass(l1999b, nClasses=3)
l1999c
plot(l1999c$map, col=CLAI, main="1999 classification") 

l2019c <- unsuperClass(l2019b, nClasses=3)
l2019c
plot(l2019c$map, col=CLAI, main="2019 classification")

#frequencies 1999
freq(l1999c$map)

# value count
#[1,]  dryarea   13273
#[2,]  ocean    27306
#[3,]  rainforest   3187

#proportion:

total <- 43766
dryarea <- 13273/total
ocean <- 27306/total
rainforest <- 3187/total

#dryarea <- 0.3032719
#ocean <- 0.623909
#rainforest <- 0.07281908


#build a dataframe
cover <- c("dry area", "ocean", "rainforest")
prop1999 <- c(dryarea, ocean, rainforest)

proportion1999 <- data.frame(cover, prop1999)
proportion1999

# cover   prop1999
#1   dry area 0.30327195
#2      ocean 0.62390897
#3 rainforest 0.07281908






#freq 2019
freq(l2019c$map)

 #  value count
#[1,]       2198 -> rainforest
#[2,]       9515  -> dry area
#[3,]      21005   -> ocean

total <- 261632
dryarea19 <- 9515/total
ocean19 <- 21005/total
rainforest19 <- 2198/total


#dryarea19 <- 0.03636788
#ocean19 <- 0.08028452
#rainforest19 <- 0.008401113


#now we are going to build a dataframe 

cover <- c("dry area 19", "ocean 19", "rainforest 19")
prop2019 <- c(dryarea19, ocean19, rainforest19)

proportion2019 <- data.frame(cover, prop2019)
proportion2019

 # cover    prop2019
#1   dry area 19 0.036367875
#2      ocean 19 0.080284522
#3 rainforest 19 0.008401113




# final ggplot

#ggplot

ggplot(proportion1999, aes(x=cover, y=prop1999, color=cover)) + geom_bar(stat="identity", fill="white")


ggplot(proportion2019, aes(x=cover, y=prop2019, color=cover)) + geom_bar(stat="identity", fill="white")

#finale dataframe

finalproportion <- data.frame(cover, prop1999, prop2019)

  #  cover   prop1999    prop2019
#1   dry area 19 0.30327195 0.036367875
#2      ocean 19 0.62390897 0.080284522
#3 rainforest 19 0.07281908 0.008401113



# now we want to plot weverything all togheter -> gridExtra packages -> we are doing multiframe by using this package 
# 1 let's assign to every ggplot a name

p1999 <- ggplot(proportion1999, aes(x=cover, y=prop1999, color=cover)) + geom_bar(stat="identity", fill="white")
p2019 <- ggplot(proportion2019, aes(x=cover, y=prop2019, color=cover)) + geom_bar(stat="identity", fill="white")

grid.arrange(p1999, p2019, nrow=1)




#FIRE DISTURBANCE
f2021 <- raster("c_gls_BA300_202107010000_GLOBE_S3_V3.0.1.nc")
f2021
c2021 <- crop(f2021, ext)
clf <- colorRampPalette(c("yellow", "red", "darkred"))(100)

plot(c2021, col=clf, main="Burnt Area")




