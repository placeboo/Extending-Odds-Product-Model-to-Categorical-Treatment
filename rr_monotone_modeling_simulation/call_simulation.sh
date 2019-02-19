#!/bin/sh
#
#
cd ~/Linbo/odds/rr_monotone_modeling_simulation/codes

outfile="sample.size.$1.seed.$2"

R CMD BATCH --vanilla "--args $1 $2" 2_simulation.R $outfile

echo "Outfile is `pwd`/$outfile" | mail -v -s "simulation finished" jiaqiyin@uw.edu
