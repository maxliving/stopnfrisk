#!/bin/sh

cd ~/Projects/stopnfrisk/data/nypd

curl -o nypd_#1.zip 'http://www.nyc.gov/html/nypd/downloads/zip/analysis_and_planning/[2003-2012].zip'

for file in nypd_*.zip
do
    unzip -u $file
done

