#############################
### nypd_data_clean.R
### The data from the NYPD come in SPSS .por format. Read these in and convert them to csv.
#############################

library(foreign)

setwd("~/Projects/stopnfrisk/data/nypd")

for (year in 2012:2003) {
    write.csv(read.spss(paste0(year,'.por')), file=paste0('nypd_',year,'.csv'))
}


