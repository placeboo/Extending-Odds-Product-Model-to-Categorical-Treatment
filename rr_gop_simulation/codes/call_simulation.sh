#!/bin/sh
#
#
cd ~/Linbo/odds/General_OP_simulation

outfile="sample.size.$1.seed.$2.txt"

R CMD BATCH --vanilla "--args $1 $2" 2_generalized_op_simulation.R $outfile

echo "Outfile is `pwd`/$outfile" | mail -v -s "simulation finished" jiaqiyin@uw.edu
rm call_simulation.sh.*