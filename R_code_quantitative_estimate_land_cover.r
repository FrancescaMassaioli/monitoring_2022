# setwd("C:/Users/franc/Desktop/lab/en")

R_code_quantitative_estimate_land_cover.r

library(raster)
library(RStoolbox) # classification
# install.packages("ggplot2")
library(ggplot2)
# install.packages("gridExtra")
library(gridExtra) # for grid.arrange plotting

setwd("~/lab/") # Linux
# setwd("C:/lab/") # Windows
# setwd("/Users/name/Desktop/lab/") # Mac

# brick
# 1 list the files available
rlist <- list.files(pattern="defor")
rlist
# now we are going to apply the  brick function because each file is composed by 3 bends, so we import the complete set of layers togheter.

# 2 lapply: apply a function to a list --> apply a function over a list 

list_rast <- lapply(rlist, brick)
list_rast
plot(list_rast[[1]])

# we are going to use simple names, you don't need the dollars because there are the 2 parentesis
# we will pass list_rast[[1]] to a new object

# defor: NIR 1, red 2, green 3 
l1992 <- list_rast[[1]]
plotRGB(l1992, r=1, g=2, b=3, stretch="lin")

l2006 <- list_rast[[2]]
plotRGB(l2006, r=1, g=2, b=3, stretch="lin")

#unsupervised classification --> to classify what are the forest and what agricultural area. we will use the Rtoolbox to make classification
# unsuperClass(img, nClasses)

l1992c <- unsuperClass(l1992, nClasses=2)
l1992c

plot(l1992c$map)

# value 1 is forest, 2 is baresoil
# How to build frequencies -> freq: is going to calculate frequencies. interative process so some pixels might be assigned to other classes

freq(l1992c$map)
value  count
[1,]     1 307242
[2,]     2  34050

# class 1 forest:  307242
# class 2 agricultural: 34050

total <- 341292
# proportion of agricultural area
propagri <- 34050/total
propforest <- 307242/total


# proportion agri : 0.09976794 - 0.10
# proportion forest : 0.9002321 - 0.90

# now we are going to build a dataframe, first explain the column:

cover <- c("Forest", "Agriculture")
prop1992 <- c(0.9002321, 0.09976794)
prop1992 <- c(propforest, propagri)

# build a data frame
proportion1992 <- data.frame(cover, prop1992)


# cover   prop1992
1      Forest 0.90023206
2 Agriculture 0.09976794

#let's plot that -> we'll use the ggplot2 packages --> aes aestethics

ggplot(proportion1992, aes(x=cover, y=prop1992, color=cover)) + geom_bar(stat="identity", fill="white")

# classification 2006
# unsupervised classification

l2006c <- unsuperClass(l2006, nClasses=2)

plot(l2006c$map)

freq(l2006c$map)

 # value  count
[1,]   FOREST  1 179277
[2,]  AGRI   2 163449

# proportion
total <- 341292
propagri2006 <- 163449/total
propforest2006 <- 179277/total

# build a dataframe

cover <- c("Forest", "Agriculture")

prop2006 <- c(propforest2006, propagri2006)

proportion2006 <- data.frame(cover, prop2006)

cover  prop2006
1      Forest 0.5252892
2 Agriculture 0.4789125

# final data.frame
proportion <- data.frame(cover, prop1992, prop2006)

# ggplot

ggplot(proportion, aes(x=cover, y=prop2006, color=cover)) + geom_bar(stat="identity", fill="white")

# now we want to plot weverything all togheter -> gridExtra packages -> we are doing multiframe by using this package 
# 1 let's assign to every ggplot a name

# p1 <- ggplot(proportion1992, aes(x=cover, y=prop1992, color=cover)) + geom_bar(stat="identity", fill="white")
# p2 <- ggplot(proportion, aes(x=cover, y=prop2006, color=cover)) + geom_bar(stat="identity", fill="white")

grid.arrange(p1, p2, nrows=1)



# NIR 1, RED 2, GREEN 3

defor1 <- brick("defor1.jpg")
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")
ggRGB(defor1, r=1, g=2, b=3, stretch="lin")

defor2 <- brick("defor2.jpg")
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")
ggRGB(defor2, r=1, g=2, b=3, stretch="lin")

par(mfrow=c(1,2))
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")

# multiframe with ggplot2 and gridExtra
p1 <- ggRGB(defor1, r=1, g=2, b=3, stretch="lin")
p2 <- ggRGB(defor2, r=1, g=2, b=3, stretch="lin")
grid.arrange(p1, p2, nrow=2)

# unsupervised classification
d1c <- unsuperClass(defor1, nClasses=2)
plot(d1c$map)
# class 1: forest
# class 2: agriculture

# set.seed() would allow you to attain the same results ...

d2c <- unsuperClass(defor2, nClasses=2)
plot(d2c$map)
# class 1: agriculture
# class 2: forest

d2c3 <- unsuperClass(defor2, nClasses=3)
plot(d2c3$map)

# frequencies
freq(d1c$map)
#   value  count
# [1,]     1 306583
# [2,]     2  34709





s1 <- 306583 + 34709

prop1 <- freq(d1c$map) / s1
# prop forest: 0.8983012
# prop agriculture: 0.1016988

s2 <- 342726
prop2 <- freq(d2c$map) / s2
# prop forest: 0.5206958
# prop agriculture: 0.4793042

# build a dataframe
cover <- c("Forest","Agriculture")
percent_1992 <- c(89.83, 10.16)
percent_2006 <- c(52.06, 47.93)

percentages <- data.frame(cover, percent_1992, percent_2006)
percentages

# let's plot them!
ggplot(percentages, aes(x=cover, y=percent_1992, color=cover)) + geom_bar(stat="identity", fill="white")
ggplot(percentages, aes(x=cover, y=percent_2006, color=cover)) + geom_bar(stat="identity", fill="white")

p1 <- ggplot(percentages, aes(x=cover, y=percent_1992, color=cover)) + geom_bar(stat="identity", fill="white")
p2 <- ggplot(percentages, aes(x=cover, y=percent_2006, color=cover)) + geom_bar(stat="identity", fill="white")

grid.arrange(p1, p2, nrow=1)

# + ylim(0,100)
