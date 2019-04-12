#!/bin/sh
#
#
cd ~/Linbo/odds/titanic 
for seed in $(seq 5)
do
	qsub -cwd ./codes/mono_call_simulation.sh $seed
done

