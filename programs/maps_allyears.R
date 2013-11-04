# maps_allyears.R
# Created 10/22/2013 by Max Livingston
# Incorporates all the years of SQF data to make yearly and combined maps.
# Data from NYPD: http://www.nyc.gov/html/nypd/html/analysis_and_planning/stop_question_and_frisk_report.shtml

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

## As far as I know, the precincts stay constant over these years (I only have one precinct shapefile anyway), so we only read in one precinct shapefile:
## Set up a map dataframe that ggplot can use (see https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles)
precinctmap <- readOGR("data/nyc_precinctmap","nypp")
precinctmap@data$id <- rownames(precinctmap@data)
## Normally I use fortify(precinctmap,region="id") but I got a weird error message when I did that, and the whole process worked when I took it out. Should look into that at some point.
precinctmap.points <- fortify(precinctmap)
precinctmap.df <- join(precinctmap.points,precinctmap@data,by="id")

## Getting nice-looking maps requires changing a bunch of ggplot2 defaults
theme.opts <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          legend.title=element_blank())

for (year in 2011:2012) {
    
    dfrm <- read.csv(paste0("data/nypd/nypd_",year,".csv"))
    ## Get everything in lower-case for consistency:
    colnames(dfrm) <- tolower(colnames(dfrm))
        
    stops <- fortify(dfrm)
    stops$long <- stops$xcoord
    stops$lat <- stops$ycoord

    ## Recode White Hisp as Black Hisp; we are going to combine this into Hisp.
    ## stops$race[stops$race == 3] <- 2
    ## stops$race <- factor(stops$race, 
    ##                      levels=c(1:6), 
    ##                      labels=c("Black","Hispanic","","White","Asian","AmInd"))
    ## stops$frisked.f <- factor(stops$frisked,levels=c(0,1))
    ## stops$arrest.f <- factor(stops$arstmade,levels=c(0,1))
    ## stops$black <- stops$race=="Black"

    
    pctmapplot <- ggplot(precinctmap.df,aes(x=long,y=lat,group=group)) + geom_path(size=.3)
    pctmapplot + geom_point(data=stops,aes(group=NULL),size=.1,color="#D55E00") + theme.opts
    ggsave(file=paste0("output/",year,"/precincts_points.pdf"))

    pcts.stops <- ggplot(stops,aes(xcoord,ycoord)) + geom_path(data=precinctmap.df,aes(x=long,y=lat,group=group),size=0.15)
    ## precincts + stop dots by arrest vs. non-arrest
    pcts.stops +
        geom_point(size=0.25,aes(color="D55E00"),subset=.(arstmade=="N")) +
            geom_point(size=0.25,color="green",subset=.(arstmade=="Y")) + theme.opts
    ggsave(paste0("output/",year,"/arrests.pdf"))

}
