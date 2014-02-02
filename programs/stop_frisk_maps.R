# stop_frisk_maps.R
# Created 8/20/2013 by Max Livingston
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
##precinctmap <- readShapePoly("data/nyc_precinctmap/nypp")

pdf("output/precincts_plain.pdf")
plot(precinctmap)
invisible(text(getSpPPolygonsLabptSlots(precinctmap), labels=as.character(precinctmap$Precinct), cex=0.4))
dev.off()

## Create precinct-level aggregates and merge them with the precinct map data
stats.by.pct <- aggregate(cbind(arstmade,pistol) ~ pct, data=dfrm, sum)
## Count of obs in each pct:
freq <- as.matrix(table(dfrm$pct)) 
stats.by.pct <- cbind(stats.by.pct, freq)
stats.by.pct$arrestrate <- 100*with(stats.by.pct, arstmade / freq)

## Normally I use fortify(precinctmap,region="id") but I got a weird error message when I did that, and the whole process worked when I took it out. Should look into that at some point.
precinctmap@data$id <- rownames(precinctmap@data)
precinctmap.points <- fortify(precinctmap)
precinctmap.df <- join(precinctmap.points,precinctmap@data,by="id")

precinctmap.df <- merge(precinctmap.df,stats.by.pct, by.x="Precinct", by.y="pct", all.x=T, a..ly=F)

## Get the midpoint of lat and lon for each precinct, to be used for labeling
cnames <- aggregate(cbind(long, lat) ~ Precinct, data=precinctmap.df, FUN=function(x) mean(range(x)))

## Now that we've readied the precinctmap polygons, we need to prepare the points (from the csv)
stops <- fortify(dfrm)
stops$long <- stops$xcoord
stops$lat <- stops$ycoord

## Recode White Hisp as Black Hisp; we are going to combine this into Hisp.
stops$race[stops$race == 3] <- 2
stops$race <- factor(stops$race, 
    levels=c(1:6), 
    labels=c("Black","Hispanic","","White","Asian","AmInd"))
stops$frisked.f <- factor(stops$frisked,levels=c(0,1))
stops$arrest.f <- factor(stops$arstmade,levels=c(0,1))
stops$arrest.f.rev <- factor(stops$arstmade,levels = rev(levels(factor(stops$arstmade))))
stops$black <- stops$race=="Black"

stops$weapon <- rowSums(stops[,c("contrabn","pistol","riflshot","asltweap","knifcuti","machgun","othrweap")]) != 0

## Mapping/exploration
## I read that it's faster if we don't map the shapefile, just the points. Gives you a decent sense of what the dfrm look like. However, I'm commenting this out now as I move away from exploration and more toward making the actual maps.
 
## ## Overall map of all entries in the dataset
## ggplot(dfrm,aes(xcoord, ycoord, color="##D55E00")) + geom_point(size=.075) 
## ## Map stops, colored by frisks vs. just stops
## ggplot(dfrm,aes(xcoord, ycoord, colour=frisked.f)) + geom_point(size=.3) 
## ## Map of stops that resulted in arrests
## map <- ggplot(dfrm[which(dfrm$arstmade==1),],aes(xcoord, ycoord)) + geom_point(size=.3)
## ggsave("output/just_arrests.png")
## ## Map showing arrests vs. non-arrests. Hard to really tell what's going on
## map <- ggplot(dfrm,aes(xcoord, ycoord, colour=arrest.f)) + geom_point(size=.3)
## ggsave("output/arrests_v1.png")
## ## Made a bit better by stacking
## map <- ggplot(dfrm) + geom_point(aes(xcoord,ycoord,color=arrest.f),size=0.3) +
##     geom_point(aes(xcoord, ycoord, colour=arrest.f), size=.3, subset=.(arstmade==1))
## ggsave("output/arrests_v2.png")


## ## Too many points! Hard to tell what's going on, so experimenting with contours:
## map <- ggplot(dfrm,aes(xcoord,ycoord))
## map + geom_density2d()
## ggsave("output/contourmap.png")
## map <- map + geom_point(size=0.3) + geom_density2d()
## ggsave("output/contourmap_wpoints.png")


## map <- ggplot(dfrm,aes(xcoord,ycoord))
## map <- map + stat_density2d(aes(fill = ..level..), geom="polygon")
## map + scale_fill_gradient2(space="rgb",low="white",high="red") + geom_point(size=0.2)

## pctmap <- ggplot(precinctmap.df,aes(long,lat,group=group)) + geom_polygon() + geom_path(color="white") + scale_fill_brewer()

## + geom_polygon(fill="white",color="grey50")


## Getting nice-looking maps requires changing a bunch of ggplot2 defaults
theme.opts <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank() ,legend.title=element_blank())

# Bring in the polygon shapefile of NYC police precincts and plot the stops on that map:
pctmapplot <- ggplot(precinctmap.df,aes(x=long,y=lat,group=group)) + geom_path(size=.3)
pctmapplot + geom_point(data=stops,aes(group=NULL),size=.1,color="#D55E00") + theme.opts
ggsave(file="output/precincts_points.pdf")
pctmapplot + geom_density2d(data=stops,aes(group=NULL,fill = ..level..)) + theme.opts
ggsave("output/precincts_density.jpg")

pcts.stops <- ggplot(stops,aes(xcoord,ycoord)) + geom_path(data=precinctmap.df,aes(x=long,y=lat,group=group),size=0.15)
# precincts + stop dots by arrest vs. non-arrest

arrests.map <- ggplot(stops,aes(x=xcoord,y=ycoord,color=arrest.f.rev)) + geom_point(size=0.15)
## arrests.map

## pcts.stops + geom_point(size=0.10,aes(color=arrest.f)) +
##     scale_color_discrete() +
##     guides(color = guide_legend(override.aes = list(size=3)))

pcts.stops +
    geom_point(size=0.25,color="#D55E00",subset=.(arstmade==0)) +
    geom_point(size=0.25,color="green",subset=.(arstmade==1)) +
    theme.opts
ggsave("output/arrests.pdf")
ggsave("output/arrests.png")

pcts.stops +
    geom_point(size=0.15,aes(color=race),subset=.(race!="AmInd")) +
    guides(color = guide_legend(override.aes = list(size=3))) + theme.opts
ggsave("output/stops_byrace.pdf")
ggsave("output/stops_byrace.png")

pcts.stops +
    geom_point(size=0.15,aes(color=weapon),subset=.(frisked==1 | searched==1)) +
    guides(color = guide_legend(override.aes = list(size=3))) + theme.opts
ggsave("output/stops_weapons.pdf")
ggsave("output/stops_weapons.png")

# precincts + density:
pctmap_density <- ggplot(stops, aes(xcoord,ycoord)) +
    stat_density2d(aes(group=NULL,fill= ..level..),geom="polygon") +
    scale_fill_gradient2(space="rgb",low="white",high="red")
pctmap_density <- pctmap_density + geom_path(data=precinctmap.df,aes(x=long,y=lat,group=group))
pctmap_density
ggsave("output/heatmap.pdf")
ggsave("output/heatmap.png")

## Choropleth using precincts:
pctmapplot <- ggplot(precinctmap.df,aes(x=long,y=lat,group=group,fill=arstmade)) +
    geom_polygon() +
    scale_fill_gradient(low="white",high="red",guide=guide_legend("Arrests")) +
    geom_path(size=0.5)
pctmapplot + theme.opts + theme(legend.title=element_text())
ggsave("output/choropleth_arrests.pdf")
ggsave("output/choropleth_arrests.png")

pctmapplot <- ggplot(precinctmap.df,aes(x=long,y=lat,group=group)) +
    geom_polygon(aes(group=group, fill=freq)) +
    scale_fill_gradient(low="white",high="red",guide=guide_legend("Stops")) +
    geom_path(size=0.5)
pctmapplot + theme.opts + theme(legend.title=element_text())
ggsave("output/choropleth_stops.pdf")
ggsave("output/choropleth_stops.png")


pctmapplot <- ggplot(precinctmap.df,aes(x=long,y=lat,group=group)) +
    geom_polygon(aes(group=group, fill=arrestrate)) +
    scale_fill_gradient(low="white",high="red",guide=guide_legend("Arrest / Stops")) +
    geom_path(size=0.5)
pctmapplot + theme.opts + theme(legend.title=element_text())
ggsave("output/choropleth_arrestrate.pdf")
ggsave("output/choropleth_arrestrate.png")

