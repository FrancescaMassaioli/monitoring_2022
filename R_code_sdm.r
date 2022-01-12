# Species Distribution Modelling

# install.packages("sdm")
# install.packages("rgdal", dependencies=T) 

library(sdm)
library(raster) # predictors -> plotting the predictors, variables
library(rgdal) # species -> used for coordinate data, vector data, an arrey of data. x and y. 

# system.file function: is showing all of the files in a certain package 
# catching the name of the file, where the file is located, external is a label

file <- system.file("external/species.shp", package="sdm")
file # the path where is the file
# let's plot the species data. recreating a file inside R -> shape file funciton, we are importing this data and explaining this is shape file
# shapefile function -> exactly as the raster function for raster file
# it will be points in space, coordinates for species and the presence or absence information
species <- shapefile(file)
plot(species, pch=19, col="red")
# pattern within the population 

...................................................

# looking at the set
species

# plot
plot(species)

# looking at the occurrences: (variables 0 or 1 in this case)
species$Occurrence

# sql: subseting the data, queering the dataset, make filters. 
# how many 1s are there? start with species
# how many occurrences are there?
presences <- species[species$Occurrence == 1,]

absences <- species[species$Occurrence == 0,]

# plot!:
plot(species, pch=19)

# plot presences 
plot(presences, pch=19, col="blue")
# how to add to a previous plot additional points? with points function
points(absences, pch=19, col="red")


plot(species[species$Occurrence == 1,],col='blue',pch=16)
points(species[species$Occurrence == 0,],col='red',pch=16)

# let's look at the predictors: look at the path

path <- system.file("external", package="sdm") 

# list the predictors: make a list with all the file, list.file function, list the file inside R

lst <- list.files(path,pattern='asc', full.names=T) #full name is require
lst

# put all the file togheter, with the stack. you use the lapply function but in this case is not deened since the data are inside the package and they have the asc extention  

preds <- stack(lst)

# now we have the species and the predictors, so 1 we can plot the predictors
# plot preds

cl <- colorRampPalette(c('blue','orange','red','yellow')) (100)
plot(preds, col=cl)

# plot presences with variables, predictors and presences, to see how species is spread over space

plot(preds$elevation, col=cl)
points(presences, pch=19)

plot(preds$temperature, col=cl)
points(presences, pch=19)

plot(preds$vegetation, col=cl)
points(presences, pch=19)

plot(preds$precipitation, col=cl)
points(presences, pch=19)

...............................................
plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,], pch=16)

plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,], pch=16)
.................................................


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
