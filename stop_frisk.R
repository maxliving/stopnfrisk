# stop_frisk.R
# Created 8/20/2013 by Max Livingston
# Last Modified: 8/20/2013
# Some basic stats/plotting of NYC Stop and Frisk dfrm
# dfrm obtained from the ACLU: http://www.nyclu.org/content/stop-and-frisk-dfrm

library(rgeos)
library(maps)
#library(gpclib)
library(maptools)

library(rgdal)
library(ggplot2)
library(plyr)
library(mapproj)

# Basic setup:
setwd("~/Projects/stopnfrisk")
dfrm <- read.csv("data/SQF_2012.csv")
precinctmap <- readOGR("data/nyc_precinctmap","nypp")

ls()
dim(dfrm)
str(dfrm)

dfrm$race <- factor(dfrm$race, 
    levels=c(1:6), 
    labels=c("Black","Black Hisp","White Hisp","White","Asian","AmInd"))
dfrm$frisked.f <- factor(dfrm$frisked,levels=c(0,1))
dfrm$arrest.f <- factor(dfrm$arstmade,levels=c(0,1))

# Summary stats:

mytable <- table(frisked)
table(machgun)
dfrm[which(machgun==1),]
dfrm[which(detailcm==4),]

dfrm$black <- dfrm$race=="Black" | dfrm$race=="Black Hisp"
tapply(dfrm$frisked, dfrm$race, mean)
tapply(dfrm$frisked, dfrm$black, mean)
table(dfrm$frisked,dfrm$black)

# Mapping/exploration
# faster if we don't map the shapefile, just the points. Gives you a decent sense of what the dfrm look like:

# Overall map of all entries in the dataset
ggplot(dfrm,aes(xcoord, ycoord)) + geom_point(size=.3) 
# Map stops, colored by frisks vs. just stops
ggplot(dfrm,aes(xcoord, ycoord, colour=frisked.f)) + geom_point(size=.3) 
# Map of stops that resulted in arrests
map <- ggplot(dfrm[which(dfrm$arstmade==1),],aes(xcoord, ycoord)) + geom_point(size=.3)
ggsave("output/just_arrests.png")
# Map showing arrests vs. non-arrests. Hard to really tell what's going on
map <- ggplot(dfrm,aes(xcoord, ycoord, colour=arrest.f)) + geom_point(size=.3)
ggsave("output/arrests_v1.png")
# Made a bit better by stacking
map <- ggplot(dfrm) + geom_point(aes(xcoord,ycoord,color=arrest.f),size=0.3) +
    geom_point(aes(xcoord, ycoord, colour=arrest.f), size=.3, subset=.(arstmade==1))
ggsave("output/arrests_v2.png")


# Too many points! Hard to tell 
map <- ggplot(dfrm,aes(xcoord,ycoord))
map + geom_density2d()
ggsave("output/contourmap.png")
map <- map + geom_point(size=0.3) + geom_density2d()
ggsave("output/contourmap_wpoints.png")

