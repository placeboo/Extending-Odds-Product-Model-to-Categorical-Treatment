#!/bin/sh
#
#

outfile="mono_sample.seed.$2.txt"

R CMD BATCH --vanilla "--args $1 $2" codes/mono_bootstrap.R $outfile

echo "Outfile is `pwd`/$outfile" | mail -v -s "simulation finished" jiaqiyin@uw.edu
