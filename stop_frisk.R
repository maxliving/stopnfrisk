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

### Basic setup
setwd("~/Projects/stopnfrisk")
dfrm <- read.csv("data/SQF_2012.csv")

### Set up a map dataframe that ggplot can use (see https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles)
precinctmap <- readOGR("data/nyc_precinctmap","nypp")
precinctmap@data$id <- rownames(precinctmap@data)
## Normally I use fortify(precinctmap,region="id") but I got a weird error message when I did that, and the whole process worked when I took it out. Should look into that at some point.
precinctmap.points <- fortify(precinctmap)
precinctmap.df <- join(precinctmap.points,precinctmap@data,by="id")

## Now that we've readied the precinctmap polygons, we need to prepare the points (from the csv)
stops <- fortify(dfrm)
stops$long <- stops$xcoord
stops$lat <- stops$ycoord


dfrm$race <- factor(dfrm$race, 
    levels=c(1:6), 
    labels=c("Black","Black Hisp","White Hisp","White","Asian","AmInd"))
dfrm$frisked.f <- factor(dfrm$frisked,levels=c(0,1))
dfrm$arrest.f <- factor(dfrm$arstmade,levels=c(0,1))
dfrm$black <- dfrm$race=="Black" | dfrm$race=="Black Hisp"

## Summary stats:
tapply(dfrm$frisked, dfrm$race, mean)
tapply(dfrm$frisked, dfrm$black, mean)
table(dfrm$frisked,dfrm$black)

## Mapping/exploration
## faster if we don't map the shapefile, just the points. Gives you a decent sense of what the dfrm look like:

## Overall map of all entries in the dataset
ggplot(dfrm,aes(xcoord, ycoord, color="##D55E00")) + geom_point(size=.075) 
## Map stops, colored by frisks vs. just stops
ggplot(dfrm,aes(xcoord, ycoord, colour=frisked.f)) + geom_point(size=.3) 
## Map of stops that resulted in arrests
map <- ggplot(dfrm[which(dfrm$arstmade==1),],aes(xcoord, ycoord)) + geom_point(size=.3)
ggsave("output/just_arrests.png")
## Map showing arrests vs. non-arrests. Hard to really tell what's going on
map <- ggplot(dfrm,aes(xcoord, ycoord, colour=arrest.f)) + geom_point(size=.3)
ggsave("output/arrests_v1.png")
## Made a bit better by stacking
map <- ggplot(dfrm) + geom_point(aes(xcoord,ycoord,color=arrest.f),size=0.3) +
    geom_point(aes(xcoord, ycoord, colour=arrest.f), size=.3, subset=.(arstmade==1))
ggsave("output/arrests_v2.png")


## Too many points! Hard to tell what's going on, so experimenting with contours:
map <- ggplot(dfrm,aes(xcoord,ycoord))
map + geom_density2d()
ggsave("output/contourmap.png")
map <- map + geom_point(size=0.3) + geom_density2d()
ggsave("output/contourmap_wpoints.png")

map <- ggplot(dfrm,aes(xcoord,ycoord))
map + geom_density2d()
ggsave("output/contourmap.png")
map <- map + geom_point(size=0.3) + geom_density2d()
ggsave("output/contourmap_wpoints.png")

map <- ggplot(dfrm,aes(xcoord,ycoord))
map <- map + stat_density2d(aes(fill = ..level..), geom="polygon")
map + scale_fill_gradient2(space="rgb",low="white",high="red") + geom_point(size=0.2)

pctmap <- ggplot(precinctmap.df,aes(long,lat,group=group)) + geom_polygon() + geom_path(color="white") + scale_fill_brewer()

+ geom_polygon(fill="white",color="grey50")

# Bring in the polygon shapefile of NYC police precincts and plot the stops on that map:
pctmapplot <- ggplot(precinctmap.df,aes(x=long,y=lat,group=group)) + geom_path(size=.3)
pctmapplot + geom_point(data=stops,aes(group=NULL),size=.1,color="#D55E00")
