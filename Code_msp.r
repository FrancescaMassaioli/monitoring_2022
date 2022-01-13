# Species Distribution Modelling

# install.packages("sdm")
# install.packages("rgdal", dependencies=T)

library(sdm)
library(raster) # predictors
library(rgdal) # species

file <- system.file("external/species.shp", package="sdm") 
species <- shapefile(file)

# looking at the set
species

# plot
plot(species)

# looking at the occurrences
species$Occurrence

# copy and then write:
plot(species[species$Occurrence == 1,],col='blue',pch=16)
points(species[species$Occurrence == 0,],col='red',pch=16)

# predictors: look at the path
path <- system.file("external", package="sdm") 

# list the predictors
lst <- list.files(path=path,pattern='asc$',full.names = T) #
lst

# stack
preds <- stack(lst)

# plot preds
cl <- colorRampPalette(c('blue','orange','red','yellow')) (100)
plot(preds, col=cl)

# plot predictors and occurrences
plot(preds$elevation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,], pch=16)

plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

# model

# set the data for the sdm
datasdm <- sdmData(train=species, predictors=preds)

# model
m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation, data=datasdm, methods = "glm")

# make the raster output layer
p1 <- predict(m1, newdata=preds) 

# plot the output
plot(p1, col=cl)
points(species[species$Occurrence == 1,], pch=16)

# add to the stack
s1 <- stack(preds,p1)
plot(s1, col=cl)

# Do you want to change names in the plot of the stack?
# Here your are!:
# choose a vector of names for the stack, looking at the previous graph, qhich are:
names(s1) <- c('elevation', 'precipitation', 'temperature', 'vegetation', 'model')
# and then replot!:
plot(s1, col=cl)
# you are done, with one line of code (as usual!)






## DAY 2
# setwd("C:/Users/franc/Desktop/lab")

# function source : read R code from a file 
source("R_code_source_sdm.r")
# in the theretical slide of SDMs we shoudl use individuals species and predictors 
preds
# temperature, vegetation, precipitation, elevetion

# now we build a model 
#1. explaining to the saftware the data we are going to use. training data : explaining to the model where the species is or is not. environmental variables are called predictors
# sdmData creating an a object with sdm 
datasdm <- sdmData(train=species, predictors=preds)

#sdm function: we make a model to predict if a species is present and how much : sdm(formula, data, methids,...)

#we have occurrence and a predictors in the y and x coordinate. points in space. relationdhip between them. y= bx + a. a is the intercept. b is called slope. 
# linear model. methods:glms: assumptions, each variable is normally distributed. GAM: Generalized additive models.

m1 <- sdm(Occurrence~temperature+elevation+precipitation+vegetation, data=datasdm, methods="glm")

#we want to see the plot of the models

#the model produces slope and intercept. is the very final application of the formula. now we use the model to make the prediction. for each pixels state what is the probability of presence based on this model. 
#apply the corrisponding slope to his predictors. 




