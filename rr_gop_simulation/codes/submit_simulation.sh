#!/bin/sh
#
#        
cd ~/Linbo/odds/General_OP_simulation
for seed in 1 2 3 4 5
do
	for sample_size in 50 100 500 1000 5000 10000 50000
	do
	qsub -cwd ./call_simulation.sh $sample_size $seed
	done
done 


