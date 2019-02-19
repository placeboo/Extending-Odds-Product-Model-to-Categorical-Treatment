#!/bin/sh
#
#
cd ~/Linbo/odds/rr_monotone_modeling_simulation/codes
seed=1

for sample_size in 50 100 500 1000 5000
do
	qsub -cwd ./call_simulation.sh $sample_size $seed
done
rm call_simulation.sh.*
