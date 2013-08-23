# stop_frisk.R
# Created 8/20/2013 by Max Livingston
# Last Modified: 8/20/2013
# Some basic stats/plotting of NYC Stop and Frisk data
# Data obtained from the ACLU: http://www.nyclu.org/content/stop-and-frisk-data

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
data <- read.csv("data/SQF_2012.csv")
dim(data)
str(data)
