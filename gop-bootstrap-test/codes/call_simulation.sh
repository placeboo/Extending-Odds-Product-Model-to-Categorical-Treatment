#!/bin/sh
#
#

outfile="sample.size.$1.seed.$2.txt"

R CMD BATCH --vanilla "--args $1 $2" 2_bootstrap.R $outfile

echo "Outfile is `pwd`/$outfile" | mail -v -s "simulation finished" jiaqiyin@uw.edu
rm call_simulation.sh.*
