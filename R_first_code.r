# This is my first code in github! Quite exciting, right? 

# Here are the input

water <- c(100, 200, 300, 400, 500)

# Marta data on fighes genomes
fishes <- c(10, 50, 60, 100, 200)



# the data we developed can be stored in a table
# a table in R is called data frame 

streams <- data.frame(water, fishes) 


streams
 

View(streams)

plot(streams) 

#now we are going import and export data!

write.table(streams, file="my_first_table.txt") 

# some collegues did send us a table How to put it into R? 

read.table(file="my_first_table.txt")

# let's assign it to an object inside R

francescatable <- read.table(file="my_first_table.txt")


# the first statistic for lazy beautiful people 

summary(ducciotable)

# infos only on fishes 

summary(francescatable$fishes)

# Histogram

hist(francescatable$fishes)

